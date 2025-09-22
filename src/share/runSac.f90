! module for executing Sac model
module runModule
  
  use namelistModule
  use ioModule
  use dateTimeUtilsModule
  use parametersType
  use runInfoType
  use forcingType
  use modelVarType
  use derivedType
  use sac_log_module
  use messagepack
  use iso_fortran_env

  implicit none
  integer :: warning_count_mass_balance = 0
  integer :: sac_log_level = LOG_LEVEL_INFO

  type, public :: sac_type
    type(namelist_type)   :: namelist
    type(runinfo_type)    :: runinfo
    type(parameters_type) :: parameters
    type(forcing_type)    :: forcing
    type(modelvar_type)   :: modelvar
    type(derived_type)    :: derived
    byte, dimension(:), allocatable :: serialization_buffer
  end type sac_type

contains

  !== Initialize the model ================================================================================

  SUBROUTINE initialize_from_file (model, config_file)
    implicit none
    
    type(sac_type), target, intent(out) :: model
    character(len=*), intent (in)       :: config_file ! namelist file from command line argument
    
    associate(namelist   => model%namelist,   &
              runinfo    => model%runinfo,    &
              parameters => model%parameters, &
              forcing    => model%forcing,    &
              modelvar   => model%modelvar,   &
              derived    => model%derived)
              
    call write_log('Initializing Sac-SMA from file', LOG_LEVEL_INFO)
    sac_log_level = get_log_level()

    !-----------------------------------------------------------------------------------------
      !  read namelist, initialize data structures and read parameters
      !-----------------------------------------------------------------------------------------
      call namelist%readNamelist(config_file)

      call runinfo%initInfo(namelist)          ! initialize run space-time info
      call forcing%initForcing(namelist)       ! initialize forcing data type/structure
      call modelvar%initModelVar(namelist)     ! initialize model states (incl. restarts)
      call parameters%initParams(namelist)     ! read and/or initialize parameters
      call derived%initDerived(namelist)       ! initialize derived values
      
      ! read parameters from input file
      call read_sac_parameters(parameters, namelist%sac_param_file, runinfo)
        
      !---------------------------------------------------------------------
      ! Open the forcing file
      ! Comp. dir. NGEN_FORCING_ACTIVE indicates Nextgen forcing is used
      !---------------------------------------------------------------------
#ifndef NGEN_FORCING_ACTIVE
      call init_forcing_files(namelist, runinfo, parameters)
#endif
      
      !---------------------------------------------------------------------
      ! If warm start is specified, read an initial state from a restart file
      ! Comp. dir. NGEN_READ_RESTART_ACTIVE indicates Nextgen sets the states
      !---------------------------------------------------------------------
#ifndef NGEN_READ_RESTART_ACTIVE
      ! we *ARE* warm-starting from a state file
      ! read in external state files and overwrites namelist state variables
      if(namelist%warm_start_run .eq. 1) then
        call read_sac_statefiles (modelvar, namelist, parameters, runinfo) 
      endif
#endif

      !---------------------------------------------------------------------
      ! Create output file and write header
      ! Compiler directive NGEN_OUTPUT_ACTIVE indicates Nextgen controls outputs
      !---------------------------------------------------------------------
#ifndef NGEN_OUTPUT_ACTIVE
      call init_output_files(namelist, runinfo, parameters)
#endif

      !---------------------------------------------------------------------
      ! If a run to write initial states is specified, open/init state files
      ! Comp. dir. NGEN_WRITE_RESTART_ACTIVE indicates Nextgen will do it
      !---------------------------------------------------------------------
#ifndef NGEN_WRITE_RESTART_ACTIVE
      ! -- If namelist write_states == 1, open state files and write header
      if(namelist%write_states .eq. 1) then
        call init_new_state_files(namelist, runinfo, parameters)
      endif
#endif

    end associate ! terminate the associate block

  END SUBROUTINE initialize_from_file                
              
             
             
  ! == Move the model ahead one time step ================================================================
  SUBROUTINE advance_in_time(model)
    type (sac_type), intent (inout) :: model     
    ! -- run sac for one time step
    call solve_sac(model)
    ! -- advance run time info
    model%runinfo%itime         = model%runinfo%itime + 1                            ! increment the integer time by 1
    !model%runinfo%time_dbl     = dble(model%runinfo%time_dbl + model%runinfo%dt)    ! increment relative model run time in seconds by DT
    model%runinfo%curr_datetime = model%runinfo%curr_datetime + model%runinfo%dt     ! increment unix model run time in seconds by DT
    call unix_to_datehr (model%runinfo%curr_datetime, model%runinfo%curr_datehr)     ! update datehr field as well
    call unix_to_date_elem (model%runinfo%curr_datetime, &
                            model%runinfo%curr_yr, model%runinfo%curr_mo, model%runinfo%curr_dy, &
                            model%runinfo%curr_hr, model%runinfo%curr_min, model%runinfo%curr_sec)
    
  END SUBROUTINE advance_in_time  


  ! == Routing to run the model for one timestep and all spatial sub-units ================================
  SUBROUTINE solve_sac(model)
    implicit none
    type (sac_type), intent (inout) :: model

    ! local parameters
    real               :: prcp_mm    ! precip as a depth (for input to sac) (mm)
    real               :: pet_mm     ! pet as a depth (for input to sac) (mm)

    integer            :: nh             ! counter for hrus
    real               :: uztwc_0, uzfwc_0
    real               :: lztwc_0, lzfsc_0, lzfpc_0
    real               :: adimc_0
    real               :: dt_mass_bal
    character(50)      :: str_real

    associate(namelist   => model%namelist,   &
              runinfo    => model%runinfo,    &
              parameters => model%parameters, &
              forcing    => model%forcing,    &
              modelvar   => model%modelvar,   &
              derived    => model%derived)
      !---------------------------------------------------------------------
      ! Read in the forcing data if NGEN_FORCING_ACTIVE is not defined
      !   will read current timestep forcing for all snowbands
      !---------------------------------------------------------------------
#ifndef NGEN_FORCING_ACTIVE
      call read_areal_forcing(namelist, parameters, runinfo, forcing)
#endif

      !---------------------------------------------------------------------
      ! call the main sac state update routine in loop over spatial sub-units
      !---------------------------------------------------------------------
      do nh=1, runinfo%n_hrus

        prcp_mm = forcing%precip(nh)*runinfo%dt   ! convert precip input to a depth per timestep
        pet_mm = forcing%pet(nh)*runinfo%dt       ! convert pet input to a depth per timestep

        uztwc_0 = modelvar%uztwc(nh)
        uzfwc_0 = modelvar%uzfwc(nh)
        lztwc_0 = modelvar%lztwc(nh)
        lzfsc_0 = modelvar%lzfsc(nh)
        lzfpc_0 = modelvar%lzfpc(nh)
        adimc_0 = modelvar%adimc(nh)
         
        call exsac( 1, &                     ! NSOLD, which isn't used
                    real(runinfo%dt), &      ! DTM, the timestep in seconds
                    ! Forcing inputs
                    prcp_mm, &               ! liquid water input (mm)
                    forcing%tair(nh), &      ! average air temperature (degC)
                    pet_mm, &                ! potential evapotranspiration (mm)
                    ! Sac parameters
                    parameters%uztwm(nh), parameters%uzfwm(nh), parameters%uzk(nh), &
                    parameters%pctim(nh), parameters%adimp(nh), parameters%riva(nh), &
                    parameters%zperc(nh), parameters%rexp(nh), parameters%lztwm(nh), &
                    parameters%lzfsm(nh), parameters%lzfpm(nh), parameters%lzsk(nh), &
                    parameters%lzpk(nh), parameters%pfree(nh), parameters%side(nh), &
                    parameters%rserv(nh), &
                    ! Sac state variables
                    modelvar%uztwc(nh), modelvar%uzfwc(nh), modelvar%lztwc(nh), &
                    modelvar%lzfsc(nh), modelvar%lzfpc(nh), modelvar%adimc(nh), &
                    ! Sac Outputs
                    modelvar%qs(nh), modelvar%qg(nh), modelvar%tci(nh), modelvar%eta(nh), &
                    modelvar%roimp(nh), modelvar%sdro(nh), modelvar%ssur(nh), &
                    modelvar%sif(nh), modelvar%bfs(nh), modelvar%bfp(nh), modelvar%bfncc(nh) )   
                                                   
        !---------------------------------------------------------------------
        ! Mass balance check
        !---------------------------------------------------------------------

        derived%precip_sum(nh) = derived%precip_sum(nh) + prcp_mm
        derived%eta_sum(nh) = derived%eta_sum(nh) + modelvar%eta(nh)
        derived%tci_sum(nh) = derived%tci_sum(nh) + modelvar%tci(nh)
        derived%bfncc_sum(nh) = derived%bfncc_sum(nh) + modelvar%bfncc(nh)

        derived%delta_uztwc_sum(nh) = derived%delta_uztwc_sum(nh) + (modelvar%uztwc(nh) - uztwc_0)
        derived%delta_uzfwc_sum(nh) = derived%delta_uzfwc_sum(nh) + (modelvar%uzfwc(nh) - uzfwc_0)
        derived%delta_lztwc_sum(nh) = derived%delta_lztwc_sum(nh) + (modelvar%lztwc(nh) - lztwc_0)
        derived%delta_lzfsc_sum(nh) = derived%delta_lzfsc_sum(nh) + (modelvar%lzfsc(nh) - lzfsc_0)
        derived%delta_lzfpc_sum(nh) = derived%delta_lzfpc_sum(nh) + (modelvar%lzfpc(nh) - lzfpc_0)
        derived%delta_adimc_sum(nh) = derived%delta_adimc_sum(nh) + (modelvar%adimc(nh) - adimc_0)

        derived%delta_storage_sum(nh) = derived%delta_uztwc_sum(nh) + derived%delta_uzfwc_sum(nh) +  &
                                        derived%delta_lztwc_sum(nh) + derived%delta_lzfsc_sum(nh) +  &
                                        derived%delta_lzfpc_sum(nh) 
        derived%mass_balance(nh) = derived%precip_sum(nh) - derived%eta_sum(nh) - derived%tci_sum(nh) -    &
                                   (derived%delta_storage_sum(nh)*(1-parameters%adimp(nh)-parameters%pctim(nh))) - &
                                   (derived%delta_adimc_sum(nh)*parameters%adimp(nh)) - derived%bfncc_sum(nh)
    
        !if(ABS(derived%mass_balance(nh)) .GT. 1.0E-5) then
        if(ABS(derived%mass_balance(nh)) .GT. 1.0E-2) then
            warning_count_mass_balance = warning_count_mass_balance + 1
            if ( (sac_log_level <= LOG_LEVEL_DEBUG) .or. (warning_count_mass_balance == 1)) then
                write(str_real, '(f20.10)' ) ABS(derived%mass_balance(nh))
                call write_log("Cumulative Mass Balance Fail. Greater than 1.0E-2", LOG_LEVEL_WARNING)
                call write_log('HRU: ' // itoa(nh) // ' mass balance (mm) = ' // trim(str_real), LOG_LEVEL_WARNING)
                if (warning_count_mass_balance == 1) then
                    if (sac_log_level .ne. LOG_LEVEL_DEBUG) then
                        call write_log("Logging this message only once. Set log level to DEBUG to see all occurrences.", LOG_LEVEL_WARNING)
                    end if
                end if
            end if
        end if

        !---------------------------------------------------------------------
        ! add results to output file if NGEN_OUTPUT_ACTIVE is undefined
        !---------------------------------------------------------------------

#ifndef NGEN_OUTPUT_ACTIVE
        call write_sac_output(namelist, runinfo, parameters, forcing, modelvar, derived, nh)
#endif
        ! === write out end-of-timestep values to STATE FILES for sac if requested in namelist ===
#ifndef NGEN_WRITE_RESTART_ACTIVE
        if(namelist%write_states .eq. 1) then
          call write_sac_statefile(runinfo, namelist, modelvar, nh)  
        end if
#endif        
      end do  ! end of spatial sub-unit (snowband) loop

    end associate ! terminate associate block
    
  END SUBROUTINE solve_sac
  
  
  !== Finalize the model ================================================================================
  SUBROUTINE cleanup(model)
    implicit none
    type(sac_type), intent(inout) :: model
    
    ! local variables
    integer         :: nh
    
    if (warning_count_mass_balance > 0) then
        call write_log('Cumulative Mass Balance Fail warning occurred ' //  itoa(warning_count_mass_balance) // ' times', LOG_LEVEL_WARNING)
    end if

    !---------------------------------------------------------------------
    ! Compiler directive NGEN_OUTPUT_ACTIVE to be defined if 
    ! Nextgen is writing model output (https://github.com/NOAA-OWP/ngen)
    !---------------------------------------------------------------------
#ifndef NGEN_OUTPUT_ACTIVE
    !call finalize_output()      ! short enough that another sub not needed unless to print some end of run info
      
    ! -- close all forcing and output files
    do nh=1, model%runinfo%n_hrus
      close(model%runinfo%forcing_fileunits(nh))
      close(model%runinfo%output_fileunits(nh))
    end do
    !
    !
    ! Now 'nh' value is (runinfo%n_hrus + 1)
    close(model%runinfo%output_fileunits(nh)) ! combined output file (basin avg)

#endif

#ifndef NGEN_WRITE_RESTART_ACTIVE
    ! -- close state files
    do nh=1, model%runinfo%n_hrus
      close(model%runinfo%state_fileunits(nh))
    end do
#endif
    !Free up serialization buffer memory
    if(allocated(model%serialization_buffer)) then
      deallocate(model%serialization_buffer)
    end if
  
  end subroutine cleanup

  SUBROUTINE new_serialization_request (model, exec_status)
    type(sac_type), intent(inout) :: model
    integer(kind=int64) :: nh !counter for HRUs
    class(msgpack), allocatable :: mp
    class(mp_arr_type), allocatable :: mp_sub_arr
    class(mp_arr_type), allocatable :: mp_arr
    byte, dimension(:), allocatable :: serialization_buffer
    integer(kind=int64), intent(out) :: exec_status

    mp = msgpack()
    mp_arr = mp_arr_type(model%runinfo%n_hrus)
    do nh=1, model%runinfo%n_hrus
        mp_sub_arr = mp_arr_type(11)
        mp_sub_arr%values(1)%obj = mp_int_type(model%runinfo%curr_yr) !curr_yr
        mp_sub_arr%values(2)%obj = mp_int_type(model%runinfo%curr_mo) !curr_mo
        mp_sub_arr%values(3)%obj = mp_int_type(model%runinfo%curr_dy) !curr_dy
        mp_sub_arr%values(4)%obj = mp_int_type(model%runinfo%curr_hr) !curr_hr
        mp_sub_arr%values(5)%obj = mp_int_type(nh) !hru number
        mp_sub_arr%values(6)%obj = mp_float_type(model%modelvar%uztwc(nh)) !uztwc
        mp_sub_arr%values(7)%obj = mp_float_type(model%modelvar%uzfwc(nh)) !uzfwc
        mp_sub_arr%values(8)%obj = mp_float_type(model%modelvar%lztwc(nh)) !lztwc
        mp_sub_arr%values(9)%obj = mp_float_type(model%modelvar%lzfsc(nh)) !lzfsc
        mp_sub_arr%values(10)%obj = mp_float_type(model%modelvar%lzfpc(nh)) !lzfpc
        mp_sub_arr%values(11)%obj = mp_float_type(model%modelvar%adimc(nh)) !adimc
        mp_arr%values(nh)%obj = mp_sub_arr
    end do

    ! pack the data
    call mp%pack_alloc(mp_arr, serialization_buffer)
    if (mp%failed()) then
        call write_log("Serialization using messagepack failed!. Error:" // mp%error_message, LOG_LEVEL_FATAL)
        exec_status = 1
    else
        exec_status = 0
        model%serialization_buffer = serialization_buffer
        call write_log("Serialization using messagepack successful!", LOG_LEVEL_INFO)
    end if
  END SUBROUTINE new_serialization_request

  SUBROUTINE deserialize_mp_buffer_old(model)
    type(sac_type), intent(inout) :: model
    class(mp_value_type), allocatable :: mpv
    class(msgpack), allocatable :: mp
    class(mp_arr_type), allocatable :: arr
    integer(kind=int64) :: index, nh, numbytes, yr, mo, dd, hr
    real(kind=real64) :: uztwc, uzfwc, lztwc, lzfsc, lzfpc, adimc
    logical :: status
    character(len=10) :: state_datehr         ! string to match date in input states
    real :: prev_datetime                     ! for reading state file
    character (len=10) :: datehr

    prev_datetime = (model%runinfo%start_datetime - model%runinfo%dt)         ! decrement unix model run time in seconds by DT
    call unix_to_datehr (dble(prev_datetime), state_datehr)                   ! create statefile datestring to match      
    
    mp = msgpack()
    index = 1
    do while(mp%is_available(model%serialization_buffer(index:)))
      call mp%unpack_buf(model%serialization_buffer(index:), mpv, numbytes)   
      if(mp%failed()) then
        call write_log("De-serialization using messagepack failed!. Error:" // mp%error_message, LOG_LEVEL_FATAL)
      else
        call get_arr_ref(mpv, arr, status)
        if(status) then
          call get_int(arr%values(1)%obj, yr, status)
          call get_int(arr%values(2)%obj, mo, status)  
          call get_int(arr%values(3)%obj, dd, status)
          call get_int(arr%values(4)%obj, hr, status)
          write(datehr ,'(I0.4,I0.2,I0.2,I0.2)') yr,mo,dd,hr !format the datestring for comparison next.
          if (datehr == state_datehr) then
            call get_int(arr%values(5)%obj, nh, status)
            ! Should the state variables be the initial model variables for the restart? 
            ! Currently, it is assigned to modelvar. But, can easily be updated to initial values.
          
            call get_real(arr%values(6)%obj, uztwc, status) !uztwc
            model%modelvar%uztwc(nh) = uztwc  
            call get_real(arr%values(7)%obj, uzfwc, status) !uzfwc
            model%modelvar%uzfwc(nh) = uzfwc
            call get_real(arr%values(8)%obj, lztwc, status) !lztwc
            model%modelvar%lztwc(nh) = lztwc
            call get_real(arr%values(9)%obj, lzfsc, status) !lzfsc
            model%modelvar%lzfsc(nh) = lzfsc
            call get_real(arr%values(10)%obj, lzfpc, status) !lzfpc
            model%modelvar%lzfpc(nh) = lzfpc
            call get_real(arr%values(11)%obj, adimc, status) !adimc
            model%modelvar%adimc(nh) = adimc   
          end if
        else
          call write_log("Serialization using messagepack failed!. Error:" // mp%error_message, LOG_LEVEL_FATAL)
        end if
      end if 
      deallocate (mpv)
      index = index + numbytes
      if(index > size(model%serialization_buffer)) then
        exit
      end if
    end do
  END SUBROUTINE deserialize_mp_buffer_old

  SUBROUTINE deserialize_mp_buffer (model)
    type(sac_type), intent(inout) :: model
    class(msgpack), allocatable :: mp
    class(mp_value_type), allocatable :: mpv
    class(mp_arr_type), allocatable :: arr
    class(mp_arr_type), allocatable :: arr_all_hrus
    integer(kind=int64) :: index, nh, yr, mo, dd, hr
    real(kind=real64) :: uztwc, uzfwc, lztwc, lzfsc, lzfpc, adimc
    logical :: status
    character(len=10) :: state_datehr         ! string to match date in input states
    real :: prev_datetime                     ! for reading state file
    character (len=10) :: datehr

    prev_datetime = (model%runinfo%start_datetime - model%runinfo%dt)         ! decrement unix model run time in seconds by DT
    call unix_to_datehr (dble(prev_datetime), state_datehr)                   ! create statefile datestring to match      
    
    mp = msgpack()
    call mp%unpack(model%serialization_buffer, mpv)
    if (is_arr(mpv)) then
      call get_arr_ref(mpv, arr_all_hrus, status) 
      if (status) then
        !may be add another check here for number of elements equal to HRUs?
        do index=1, mpv%numelements()
          call get_arr_ref(arr_all_hrus%values(index)%obj,arr,status)
          if (status) then
            call get_int(arr%values(1)%obj, yr, status)
            call get_int(arr%values(2)%obj, mo, status)  
            call get_int(arr%values(3)%obj, dd, status)
            call get_int(arr%values(4)%obj, hr, status)
            write(datehr ,'(I0.4,I0.2,I0.2,I0.2)') yr,mo,dd,hr !format the datestring for comparison next.
            if (datehr == state_datehr) then
              call get_int(arr%values(5)%obj, nh, status)
              ! Should the state variables be the initial model variables for the restart? 
              ! Currently, it is assigned to modelvar. But, can easily be updated to initial values.
            
              call get_real(arr%values(6)%obj, uztwc, status) !uztwc
              model%modelvar%uztwc(nh) = uztwc  
              call get_real(arr%values(7)%obj, uzfwc, status) !uzfwc
              model%modelvar%uzfwc(nh) = uzfwc
              call get_real(arr%values(8)%obj, lztwc, status) !lztwc
              model%modelvar%lztwc(nh) = lztwc
              call get_real(arr%values(9)%obj, lzfsc, status) !lzfsc
              model%modelvar%lzfsc(nh) = lzfsc
              call get_real(arr%values(10)%obj, lzfpc, status) !lzfpc
              model%modelvar%lzfpc(nh) = lzfpc
              call get_real(arr%values(11)%obj, adimc, status) !adimc
              model%modelvar%adimc(nh) = adimc   
            end if
          else
            call write_log("Serialization using messagepack (internal array) failed!. Error:" // mp%error_message, LOG_LEVEL_FATAL)
          end if
        end do
      else
          call write_log("Serialization using messagepack (external array) failed!. Error:" // mp%error_message, LOG_LEVEL_FATAL)
      end if
    end if

    deallocate (mpv)
  
  END SUBROUTINE deserialize_mp_buffer

end module runModule              

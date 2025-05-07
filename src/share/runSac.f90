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

  implicit none

  type, public :: sac_type
    type(namelist_type)   :: namelist
    type(runinfo_type)    :: runinfo
    type(parameters_type) :: parameters
    type(forcing_type)    :: forcing
    type(modelvar_type)   :: modelvar
    type(derived_type)    :: derived
  end type sac_type

contains

  !== Initialize the model ================================================================================

  SUBROUTINE initialize_from_file (model, config_file)
    implicit none
    
    type(sac_type), target, intent(out) :: model
    character(len=*), intent (in)          :: config_file ! namelist file from command line argument
    
    associate(namelist   => model%namelist,   &
              runinfo    => model%runinfo,    &
              parameters => model%parameters, &
              forcing    => model%forcing,    &
              modelvar   => model%modelvar,   &
              derived    => model%derived)
              
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
            !call write_log("WARNING: Cumulative Mass Balance Fail", LOG_LEVEL_SEVERE)
            print*, 'WARNING: Cumulative Mass Balance Fail'
            print*, 'HRU: ', nh
            print*, "mass balance (mm) = ",derived%mass_balance(nh)

            write(str_real, '(f20.10)' ) derived%mass_balance(nh)
            !call write_log('HRU: ' // itoa(nh) // ' mass balance (mm) = ' // str_real, LOG_LEVEL_SEVERE)
            !call write_log('HRU: ' // itoa(nh) // ' mass balance (mm) = ' // rtoa(derived%mass_balance(nh)), LOG_LEVEL_SEVERE)
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
  
  end subroutine cleanup

end module runModule              

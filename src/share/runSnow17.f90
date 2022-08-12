! module for executing Snow17 model
module runModule
  
  use namelistModule
  use ioModule
  use dateTimeUtilsModule
  use parametersType
  use runInfoType
  use forcingType
  use modelVarType

  implicit none

  type, public :: snow17_type
    type(namelist_type)   :: namelist
    type(runinfo_type)    :: runinfo
    type(parameters_type) :: parameters
    type(forcing_type)    :: forcing
    type(modelvar_type)   :: modelvar
  end type snow17_type

contains

  !== Initialize the model ================================================================================

  SUBROUTINE initialize_from_file (model, config_file)
    implicit none
    
    type(snow17_type), target, intent(out) :: model
    character(len=*), intent (in)          :: config_file ! namelist file from command line argument
    
    associate(namelist   => model%namelist,   &
              runinfo    => model%runinfo,    &
              parameters => model%parameters, &
              forcing    => model%forcing,    &
              modelvar   => model%modelvar)
              
      !-----------------------------------------------------------------------------------------
      !  read namelist, initialize data structures and read parameters
      !-----------------------------------------------------------------------------------------
      call namelist%readNamelist(config_file)

      call runinfo%initInfo(namelist)          ! initialize run space-time info
      call forcing%initForcing(namelist)       ! initialize forcing data type/structure
      call modelvar%initModelVar(namelist)     ! initialize model states (incl. restarts)
      call parameters%initParams(namelist)     ! read and/or initialize parameters
      
      ! read parameters from input file
      call read_snow17_parameters(parameters, namelist%snow17_param_file, runinfo)
        
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
        call read_snow17_statefiles (modelvar, namelist, parameters, runinfo) 
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
    type (snow17_type), intent (inout) :: model
    
    !print*, 'current time: ', model%runinfo%curr_datehr

    ! -- run snow17 for one time step
    call solve_snow17(model)

    ! -- advance run time info
    model%runinfo%itime         = model%runinfo%itime + 1                            ! increment the integer time by 1
    !model%runinfo%time_dbl     = dble(model%runinfo%time_dbl + model%runinfo%dt)    ! increment relative model run time in seconds by DT
    model%runinfo%curr_datetime = model%runinfo%curr_datetime + model%runinfo%dt     ! increment unix model run time in seconds by DT
    call unix_to_datehr (model%runinfo%curr_datetime, model%runinfo%curr_datehr)     ! update datehr field as well
    call unix_to_date_elem (model%runinfo%curr_datetime, model%runinfo%curr_yr, model%runinfo%curr_mo, model%runinfo%curr_dy, &
                            model%runinfo%curr_hr, model%runinfo%curr_min, model%runinfo%curr_sec)
    
  END SUBROUTINE advance_in_time
  

  ! == Routing to run the model for one timestep and all spatial sub-units ================================
  SUBROUTINE solve_snow17(model)
    type (snow17_type), intent (inout) :: model
    
    ! local parameters
    integer            :: nh             ! counter for snowbands

    associate(namelist   => model%namelist,   &
              runinfo    => model%runinfo,    &
              parameters => model%parameters, &
              forcing    => model%forcing,    &
              modelvar   => model%modelvar)
    
      !---------------------------------------------------------------------
      ! Read in the forcing data if NGEN_FORCING_ACTIVE is not defined
      !   will read current timestep forcing for all snowbands
      !---------------------------------------------------------------------
#ifndef NGEN_FORCING_ACTIVE
      call read_areal_forcing(namelist, parameters, runinfo, forcing)
#endif

      !---------------------------------------------------------------------
      ! call the main snow17 state update routine in loop over spatial sub-units
      !---------------------------------------------------------------------
      do nh=1, runinfo%n_hrus

        call exsnow19(int(runinfo%dt), int(runinfo%dt/3600), runinfo%curr_dy, runinfo%curr_mo, runinfo%curr_yr, &
    	  ! SNOW17 INPUT AND OUTPUT VARIABLES
  	      forcing%precip(nh), forcing%tair(nh), modelvar%raim(nh), modelvar%sneqv(nh), modelvar%snow(nh), modelvar%snowh(nh), &
    	  ! SNOW17 PARAMETERS
          !ALAT,SCF,MFMAX,MFMIN,UADJ,SI,NMF,TIPM,MBASE,PXTEMP,PLWHC,DAYGM,ELEV,PA,ADC
  	      parameters%latitude(nh), parameters%scf(nh), parameters%mfmax(nh), parameters%mfmin(nh), &
          parameters%uadj(nh), parameters%si(nh), parameters%nmf(nh), parameters%tipm(nh), parameters%mbase(nh), &
          parameters%pxtemp(nh), parameters%plwhc(nh), parameters%daygm(nh), parameters%elev(nh), forcing%pa(nh), &
          parameters%adc(:,nh), &
          ! SNOW17 CARRYOVER VARIABLES
  		  modelvar%cs(:,nh), modelvar%tprev(nh) )             

        !---------------------------------------------------------------------
        ! add results to output file if NGEN_OUTPUT_ACTIVE is undefined
        !---------------------------------------------------------------------
#ifndef NGEN_OUTPUT_ACTIVE
        call write_snow17_output(namelist, runinfo, parameters, forcing, modelvar, nh)
#endif
        
        ! === write out end-of-timestep values to STATE FILES for snow17 if requested in namelist ===
#ifndef NGEN_WRITE_RESTART_ACTIVE
        if(namelist%write_states .eq. 1) then
          call write_snow17_statefile(runinfo, namelist, modelvar, nh)  
        end if
#endif        
    
      end do  ! end of spatial sub-unit (snowband) loop

    end associate ! terminate associate block
    
  END SUBROUTINE solve_snow17
  
  
  !== Finalize the model ================================================================================
  SUBROUTINE cleanup(model)
    implicit none
    type(snow17_type), intent(inout) :: model
    
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

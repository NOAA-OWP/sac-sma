module ioModule
  
  use dateTimeUtilsModule
  use parametersType
  use runInfoType
  use forcingType
  use modelVarType
  
  implicit none
  
contains

  subroutine read_snow17_parameters(this, param_file_name, runinfo)
    use runInfoType
    use dateTimeUtilsModule
    implicit none
    !use nrtype
   
    ! input/output variables
    class(runinfo_type), intent(in)        :: runinfo
    character(len=1024), intent(in)        :: param_file_name
    class(parameters_type), intent(inout)  :: this
  
    ! local variables
    character(len=400)      :: readline
    character(len=50)		:: param
    integer    	            :: ios=0   ! specify i4b with nrtype?
    integer                 :: pos
    integer                 :: n_params_read, nh  ! counters
  
    ! open parameter file
    open(unit=51,file=trim(param_file_name),status='old')
  
    print*, 'Reading Snow17 parameters'
  
    ! --- now loop through parameter file and assign parameters 
    n_params_read = 0
    do while(ios .eq. 0)
      read(unit=51,FMT='(A)',IOSTAT=ios) readline
  
      if(ios == 0) then   ! means 'readline' was from the file
        !print*, '  ',trim(readline)
  
        ! Find the first instance of whitespace in line read. Split label vs data.
        pos      = scan(readline, '    ')
        param    = trim(readline(1:pos))
        readline = readline(pos+1:)  ! shorten readline to include only data
  
        ! assign line to the correct parameter array & type
        ! (following http://jblevins.org/log/control-file)
        ! should automatically read multiple values into the variable arrays if n_hrus > 1
        select case (param)
         case ('hru_id')
            read(readline, *, iostat=ios) this%hru_id
            n_params_read = n_params_read + 1
          case ('hru_area')
            read(readline, *, iostat=ios) this%hru_area
            n_params_read = n_params_read + 1
          case ('latitude')
            read(readline, *, iostat=ios) this%latitude
            n_params_read = n_params_read + 1
          case ('elev')
            read(readline, *, iostat=ios) this%elev
            n_params_read = n_params_read + 1
          case ('mfmax')
            read(readline, *, iostat=ios) this%mfmax
            n_params_read = n_params_read + 1
          case ('mfmin')
            read(readline, *, iostat=ios) this%mfmin
            n_params_read = n_params_read + 1
          case ('scf')
            read(readline, *, iostat=ios) this%scf
            n_params_read = n_params_read + 1
          case ('uadj')
            read(readline, *, iostat=ios) this%uadj
            n_params_read = n_params_read + 1
          case ('si')
            read(readline, *, iostat=ios) this%si
            n_params_read = n_params_read + 1
          case ('pxtemp')
            read(readline, *, iostat=ios) this%pxtemp
            n_params_read = n_params_read + 1
          case ('nmf')
            read(readline, *, iostat=ios) this%nmf
            n_params_read = n_params_read + 1
          case ('tipm')
            read(readline, *, iostat=ios) this%tipm
            n_params_read = n_params_read + 1
          case ('mbase')
            read(readline, *, iostat=ios) this%mbase
            n_params_read = n_params_read + 1
          case ('plwhc')
            read(readline, *, iostat=ios) this%plwhc
            n_params_read = n_params_read + 1
          case ('daygm')
            read(readline, *, iostat=ios) this%daygm
            n_params_read = n_params_read + 1
          case ('adc1')
            !read(readline, *, iostat=ios) this%adc(:,1)
            read(readline, *, iostat=ios) this%adc(1,:)
            n_params_read = n_params_read + 1
          case ('adc2')
            !read(readline, *, iostat=ios) this%adc(:,2)
            read(readline, *, iostat=ios) this%adc(2,:)
            n_params_read = n_params_read + 1
          case ('adc3')
            read(readline, *, iostat=ios) this%adc(3,:)
            n_params_read = n_params_read + 1
          case ('adc4')
            read(readline, *, iostat=ios) this%adc(4,:)
            n_params_read = n_params_read + 1
          case ('adc5')
            read(readline, *, iostat=ios) this%adc(5,:)
            n_params_read = n_params_read + 1
          case ('adc6')
            read(readline, *, iostat=ios) this%adc(6,:)
            n_params_read = n_params_read + 1
          case ('adc7')
            read(readline, *, iostat=ios) this%adc(7,:)
            n_params_read = n_params_read + 1
          case ('adc8')
            read(readline, *, iostat=ios) this%adc(8,:)
            n_params_read = n_params_read + 1
          case ('adc9')
            read(readline, *, iostat=ios) this%adc(9,:)
            n_params_read = n_params_read + 1
          case ('adc10')
            read(readline, *, iostat=ios) this%adc(10,:)
            n_params_read = n_params_read + 1
          case ('adc11')
            read(readline, *, iostat=ios) this%adc(11,:)
            n_params_read = n_params_read + 1
          case default
            print *, 'Parameter ',param,' not recognized in snow file'
        end select
  
      end if
  
    end do
    close(unit=51)
  
    ! quick check on completeness
    if(n_params_read /= 26) then
      print *, 'Read ', n_params_read , ' SNOW17 params, but need 26.  Quitting.'; stop
    end if
    
    ! calculate derived parameters
    this%total_area = 0.0
    do nh=1, runinfo%n_hrus
      this%total_area = this%total_area + this%hru_area(nh)
    end do
    
    return
  end subroutine read_snow17_parameters

  ! ==== Open forcings files and read to start of first record
  SUBROUTINE init_forcing_files(namelist, runinfo, parameters)
    implicit none
    type (namelist_type),    intent(in)   :: namelist
    type (runinfo_type),     intent(in)   :: runinfo
    type (parameters_type),  intent(in)   :: parameters
    
    ! local variables
    character*480  :: filename
    logical        :: lexist ! logical for whether the file specified by filename exists
    integer        :: ierr=0   ! error code returned by open(iostat = ierr)
    integer        :: nh     ! loop counter
    
    ! local variables
    integer				:: yr, mnth, dy, hr, found_start, ios, skipcount
    real			    :: pcp, tav

    ! --- code ------------------------------------------------------------------
    print*, 'Initializing forcing files'
    found_start = 0
    do nh=1, runinfo%n_hrus

      ! make filename to read
      filename = trim(namelist%forcing_root) // trim(parameters%hru_id(nh)) // ".csv"

      !  Check if the specified file exists
      inquire(file = trim(filename), exist = lexist)
      if (.not. lexist) then
         write(*,'(/," ***** Problem *****")')
         write(*,'(" ***** File ''", A, "'' does not exist.")') trim(filename)
         write(*,'(" ***** Check the forcing file specified as a command-line argument",/)')
         stop ":  ERROR EXIT"
      endif
    
      ! Open the forcing file 
      open(runinfo%forcing_fileunits(nh), file = trim(filename), form = 'formatted', action = 'read', iostat = ierr)
      if (ierr /= 0) then
         write(*,'("Problem opening file ''", A, "''")') trim(filename)
         stop ":  ERROR EXIT"
      endif
      
      ! Skip 1-line header
      read(runinfo%forcing_fileunits(nh), *)
      
      ! advance to first record needed in simulation 
      skipcount = 0
      do while(ios .ge. 0)
        ! forcing could have any format (not fixed width)
        read (UNIT=runinfo%forcing_fileunits(nh), FMT=*, IOSTAT=ierr) yr, mnth, dy, hr, pcp, tav

        if(yr .eq. runinfo%start_year .and. mnth .eq. runinfo%start_month .and. dy .eq. runinfo%start_day .and. hr .eq. runinfo%start_hour) then
          found_start = found_start + 1
          exit    ! break out of the loop
        end if
        
        skipcount = skipcount + 1
      end do
      
      if(nh .eq. 1) then
        print*, ' -- skipped ', skipcount ,' initial records in forcing files'
      endif 
      
      ! backspace the file to the previous record
      backspace runinfo%forcing_fileunits(nh)

    end do  ! end loop over snow bands
    
    ! error out if start of any forcing file is not found
    if (found_start /= runinfo%n_hrus) then
      print*, 'ERROR: found the starting date in only', found_start, ' out of', runinfo%n_hrus, ' forcing files.  Quitting.'; stop
    endif

  END SUBROUTINE init_forcing_files
  
  ! subroutine used in read_areal_forcing_vec() to add pressure to forcing data structure
  subroutine sfc_pressure(elevation, sfc_pres)
    use nrtype
    use constants, only: sfc_pres_a,sfc_pres_b,sfc_pres_c,sfc_pres_d,sfc_pres_e

    implicit none

    real, intent(in)   :: elevation
    real, intent(out)  :: sfc_pres
  
    ! sfc pres in hPa
    sfc_pres = sfc_pres_a * (sfc_pres_b - (sfc_pres_c * (elevation/100.0)) &
               + (sfc_pres_d*((elevation/100.0)**sfc_pres_e)))    

    return
  end subroutine sfc_pressure
  

  ! ==== read multiple snowband forcings one timestep at a time
  !      assumes files are already opened and advanced to timestep of interest
  subroutine read_areal_forcing(namelist, parameters, runinfo, forcing)
    !use nrtype
    use namelistModule 
    implicit none

    ! input and inout variables
    type (namelist_type),    intent(in)      :: namelist
    type (parameters_type),  intent(in)      :: parameters
    type (runinfo_type),     intent(inout)   :: runinfo
    type (forcing_type),     intent(inout)   :: forcing
  
    ! local variables
    integer				                     :: nh, ierr=0
    integer				                     :: yr, mnth, dy, hr
    character(len=10)                        :: forcing_datehr
    
    !print*, "---"; print*, 'Current run datehr is ', runinfo%curr_datehr
  
    ! loop over sub-areas and read their forcings
    do nh=1, runinfo%n_hrus

      ! read one record from already open files and check success
      read (UNIT=runinfo%forcing_fileunits(nh), FMT=*, IOSTAT=ierr) yr, mnth, dy, hr, forcing%precip(nh), forcing%tair(nh)
      if(ierr /= 0) then
        print*, 'ERROR:  failed to read forcing from file', trim(namelist%forcing_root) // trim(parameters%hru_id(nh))
        STOP
      end if

      ! check forcing date against run date (in readable format)
      write(forcing_datehr ,'(I0.4,I0.2,I0.2,I0.2)') yr, mnth, dy, hr
      !print*, 'Read forcing datehr ', forcing_datehr

      if(forcing_datehr /= runinfo%curr_datehr) then
        print*, 'ERROR: forcing datehr: ',forcing_datehr, ' does not match curr_datehr of run :', runinfo%curr_datehr
        STOP
      end if 

      ! update other forcing fields (derived)
      ! NOTE: this is written now for a single temperature input (tair), but the standard for running snow17+sac is tmin/tmax
      !       we could return to the standard though a namelist option if needed
      forcing%precip_scf(nh) = forcing%precip(nh) * parameters%scf(nh)   ! scale input precip by snow correction factor
                                                                         ! (note: this is for output; model input 
                                                                         ! precip is scaled in pack19()

      call sfc_pressure(parameters%elev(nh), forcing%pa(nh))             ! fill in the surface pressure field                   
                                                        
    end do  ! end loop across snowbands (hrus)
    
    return
  END subroutine read_areal_forcing  

  ! ==== Open output files and write header
  SUBROUTINE init_output_files(namelist, runinfo, parameters)
    implicit none
    type (namelist_type),    intent(in)   :: namelist
    type (runinfo_type),     intent(in)   :: runinfo
    type (parameters_type),  intent(in)   :: parameters
    
    ! local variables
    character*256  :: filename
    logical        :: lexist ! logical for whether the file specified by filename exists
    integer        :: ierr   ! error code returned by open(iostat = ierr)
    integer        :: nh     ! loop counter

    ! --- code ------------------------------------------------------------------

    print*, 'Initializing output files'

    ! Open the main basin-average output file and write header
    filename = trim(namelist%output_root) // trim(namelist%main_id)	// '.txt'
    open(runinfo%output_fileunits(1), file = trim(filename), form = 'formatted', action = 'write', status='replace', iostat = ierr)
    if (ierr /= 0) then
      write(*,'("Problem opening file ''", A, "''")') trim(filename)
      stop ":  ERROR EXIT"
    endif
    write(runinfo%output_fileunits(1),'(A)') 'year mo dy hr tair precip precip*scf sneqv snowh raim '   ! header

    ! if user setting is to write out information for each snowband, open the individual files
    if (namelist%output_hrus == 1) then
      do nh=1, runinfo%n_hrus

        ! make filename to read
        filename = trim(namelist%output_root) // trim(parameters%hru_id(nh)) // '.txt'	

        write(*,*) "open fileunit: ", nh+1
        ! Open the output files
        open(runinfo%output_fileunits(nh+1), file = trim(filename), form = 'formatted', action = 'write', status='replace', iostat = ierr)
        if (ierr /= 0) then
          write(*,'("Problem opening file ''", A, "''")') trim(filename)
          stop ":  ERROR EXIT"
        endif
      
        ! Write 1-line header
        write(runinfo%output_fileunits(nh+1),'(A)') 'year mo dy hr tair precip precip*scf sneqv snowh raim '
        
      end do  ! end loop over sub-units
      
    end if   
    
  END SUBROUTINE init_output_files
  
  ! ==== Open new state (restart) files and write header
  SUBROUTINE init_new_state_files(namelist, runinfo, parameters)
    implicit none
    type (namelist_type),    intent(in)   :: namelist
    type (runinfo_type),     intent(in)   :: runinfo
    type (parameters_type),  intent(in)   :: parameters
    
    ! local variables
    character*256  :: filename
    logical        :: lexist ! logical for whether the file specified by filename exists
    integer        :: ierr   ! error code returned by open(iostat = ierr)
    integer        :: nh     ! loop counter

    ! --- code ------------------------------------------------------------------
    
    print*, 'Initializing new restart files'

    ! if user setting is to write out state files, open one for each snowband and write header row
    if (namelist%write_states == 1) then

      do nh=1, runinfo%n_hrus

        ! make filename  
        filename = trim(namelist%snow_state_out_root) // trim(parameters%hru_id(nh)) // '.txt'	

        ! Open the output files
        open(runinfo%state_fileunits(nh), file = trim(filename), form = 'formatted', action = 'write', status='replace', iostat = ierr)
        if (ierr /= 0) then
          write(*,'("Problem opening file ''", A, "''")') trim(filename)
          stop ":  ERROR EXIT"
        endif
      
        ! Write 1-line header
        write(runinfo%state_fileunits(nh),'(A)') &
         'datehr tprev cs1 cs2 cs3 cs4 cs5 cs6 cs7 cs8cs9 cs10 cs11 cs12 cs13 cs14 cs15 cs16 cs17 cs18 cs19'
  
      end do  ! end loop over sub-units
    end if   
    
  END SUBROUTINE init_new_state_files
  
  ! === write state information for one timestep ===
  subroutine write_snow17_statefile(runinfo, namelist, modelvar, n_curr_hru)
    implicit none   
    type (namelist_type),    intent(in)     :: namelist
    type (runinfo_type),     intent(in)     :: runinfo
    type (modelvar_type),    intent(in)     :: modelvar
    integer, intent(in)                     :: n_curr_hru
    
    ! local variables
    integer        :: ierr   ! error code returned by open(iostat = ierr)
 
    ! write fixed-width format line of state file for current timesstep and sub-unit
    41 FORMAT(I0.4, 3(I0.2), 20(F20.12))    ! use maximum precision (for double)
    write(runinfo%state_fileunits(n_curr_hru), 41, iostat=ierr) runinfo%curr_yr, runinfo%curr_mo, runinfo%curr_dy, runinfo%curr_hr, &
          modelvar%tprev(n_curr_hru), modelvar%cs(:,n_curr_hru)
    if(ierr /= 0) then
      print*, 'ERROR writing state file information for sub-unit ', n_curr_hru; stop
    endif
    
    return
  end subroutine write_snow17_statefile
  

  ! === read state information for one timestep before start of run ===
  subroutine read_snow17_statefiles (modelvar, namelist, parameters, runinfo) 
    !use nrtype
    !use def_namelists, only: snow_state_in_root
    implicit none
  
    ! input variables
    type (parameters_type), intent(in)        :: parameters
    type (namelist_type), intent(in)          :: namelist
    type (runinfo_type), intent(in)           :: runinfo
  
    ! [in]/output variables  (Note:  types before were real(sp)
    type (modelvar_type), intent(inout)       :: modelvar
   
    ! local variables
    integer               :: hru
    integer	              :: ios = 0
    character(len=480)    :: state_filename
    character(len=10)     :: statefile_datehr
    character(len=10)	  :: state_datehr         ! string to match date in input states
    real                  :: prev_datetime        ! for reading state file
    integer               :: states_found         ! counter to match hrus
    
    ! ---- code -----
    print*, 'Reading restart files'
    
    ! starting statefiles match format of statefile outputs (date then variables)
    !   statefile read looks for matching date timestep before run start because states are written at end of timestep
    prev_datetime = (runinfo%start_datetime - runinfo%dt)         ! decrement unix model run time in seconds by DT
    call unix_to_datehr (dble(prev_datetime), state_datehr)    ! create statefile datestring to match
    print*, ' -- state datehr: ', state_datehr
    
    ! loop over hrus and read and store initial state values
    states_found = 0          ! set counter
    do hru=1, runinfo%n_hrus
  
      ! make state filename
      state_filename = trim(namelist%snow_state_in_root) // trim(parameters%hru_id(hru)) // '.txt'
      open(unit=95,FILE=trim(state_filename), FORM='formatted', status='old')
      !print*, ' -- reading snow state file: ', trim(state_filename)
  
      ! format for input is an unknown number of rows with 20 data columns (1 tprev, 19 for cs)
      !   the first column is the datestring; neg ios means end of file; pos means something wrong

      ! skip header row
      read(95, *, IOSTAT=ios)
      
      ! read each row and check to see if the date matches the initial state date
      do while(ios .eq. 0)
  
        read(95, *, IOSTAT=ios) statefile_datehr, modelvar%tprev(hru), modelvar%cs(:,hru)
  
        ! checks either for real date or special keyword identifying the state to use
        !   this functionality facilitates ESP forecast initialization
        if(statefile_datehr==state_datehr .or. statefile_datehr=='FcstDate') then
          states_found = states_found + 1
          close(unit=95)
          exit               ! break out of reading loop if state found
        end if
        
      end do  ! end loop to read state file for one hru        
    end do   ! end loop over one or more hrus (eg, snowbands)
    
    ! check to make sure enough states on correct dates were found
    if (states_found /= runinfo%n_hrus) then 
      print*, 'ERROR:  matching state not found in snow17 restart file.  Looking for state date: ', state_datehr
      print*, '  -- last state read was on: ', statefile_datehr
      print*, 'Stopping.  Check inputs!'; stop
    endif
    
    return
  
  end subroutine read_snow17_statefiles
  

  ! === write output for one timestep ===
  ! assumes that files have been opened and header already written
  SUBROUTINE write_snow17_output(namelist, runinfo, parameters, forcing, modelvar, n_curr_hru)
    implicit none
    type (namelist_type),    intent(in)     :: namelist
    type (runinfo_type),     intent(in)     :: runinfo
    type (parameters_type),  intent(in)     :: parameters
    integer, intent(in)                     :: n_curr_hru   ! number of the current hru being simulated
    
    ! forcing & modelvar are because the combined variables get updated here
    type (forcing_type),     intent(inout)  :: forcing
    type (modelvar_type),    intent(inout)  :: modelvar
    
    ! local variables
    integer        :: ierr   ! error code returned by open(iostat = ierr)
    integer        :: nh     ! loop counter
  
    ! ==== WRITE output for current area simulation ====
    ! Note:  write order should match header written by open_and_init_output_files()
    
    32 FORMAT(I4.4, 3(1x,I2.2), 6(F10.3))

    ! if user setting is to write out information for each snowband, open the individual files
    if (namelist%output_hrus == 1 .and. runinfo%n_hrus > 1) then
      write(runinfo%output_fileunits(n_curr_hru+1), 32, iostat=ierr) runinfo%curr_yr, runinfo%curr_mo, runinfo%curr_dy, runinfo%curr_hr, &
            forcing%tair(n_curr_hru), forcing%precip(n_curr_hru), forcing%precip(n_curr_hru)*parameters%scf(n_curr_hru), &
            modelvar%sneqv(n_curr_hru)*1000., modelvar%snowh(n_curr_hru), modelvar%raim(n_curr_hru)
      if(ierr /= 0) then
        print*, 'ERROR writing output information for basin average'; stop
      endif            
    end if  ! IF case for writing HRU-specific output to file (not including states)

    ! ==== if all snowbands have been run, sum across snowbands with weighting for snowband area ====
    
    ! initialize for timestep
    forcing%tair_comb       = 0.0
    forcing%precip_comb     = 0.0
    forcing%precip_scf_comb = 0.0
    modelvar%sneqv_comb     = 0.0
    modelvar%snowh_comb     = 0.0
    modelvar%raim_comb      = 0.0
        
    if (n_curr_hru .eq. runinfo%n_hrus) then 
      do nh=1, runinfo%n_hrus
        forcing%tair_comb        = forcing%tair_comb + forcing%tair(nh) * parameters%hru_area(nh)
        forcing%precip_comb      = forcing%precip_comb + forcing%precip(nh) * parameters%hru_area(nh)
        forcing%precip_scf_comb  = forcing%precip_scf_comb + forcing%precip(nh) * parameters%hru_area(nh) * parameters%scf(nh)
        modelvar%sneqv_comb      = modelvar%sneqv_comb + modelvar%sneqv(nh) * parameters%hru_area(nh) 
        modelvar%snowh_comb      = modelvar%snowh_comb + modelvar%snowh(nh) * parameters%hru_area(nh) 
        modelvar%raim_comb       = modelvar%raim_comb + modelvar%raim(nh) * parameters%hru_area(nh)
      end do

      ! take average of weighted sum of HRU areas
      forcing%tair_comb        = forcing%tair_comb / parameters%total_area
      forcing%precip_comb      = forcing%precip_comb / parameters%total_area
      forcing%precip_scf_comb  = forcing%precip_scf_comb / parameters%total_area
      modelvar%sneqv_comb      = modelvar%sneqv_comb / parameters%total_area
      modelvar%snowh_comb      = modelvar%snowh_comb / parameters%total_area
      modelvar%raim_comb       = modelvar%raim_comb / parameters%total_area

      ! -- write out combined file that is similar to each area file, but add flow variable in CFS units
      write(runinfo%output_fileunits(1), 32, iostat=ierr) runinfo%curr_yr, runinfo%curr_mo, runinfo%curr_dy, runinfo%curr_hr, &
            forcing%tair_comb, forcing%precip_comb, forcing%precip_scf_comb, &
            modelvar%sneqv_comb*1000.0, modelvar%snowh_comb, modelvar%raim_comb
      if(ierr /= 0) then
        print*, 'ERROR writing output information for sub-unit ', n_curr_hru; stop
      endif
    endif 

    return
  END SUBROUTINE write_snow17_output

  
end module ioModule

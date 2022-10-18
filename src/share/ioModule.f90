module ioModule
  
  use dateTimeUtilsModule
  use parametersType
  use runInfoType
  use forcingType
  use modelVarType
  use derivedType
  
  implicit none
  
contains

  subroutine read_sac_parameters(this, param_file_name, runinfo)
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
    character(len=50)	    :: param
    integer    	            :: ios=0   ! specify i4b with nrtype?
    integer                 :: pos
    integer                 :: n_params_read, nh  ! counters
  
    ! open parameter file
    open(unit=51,file=trim(param_file_name),status='old')
  
    print*, 'Reading Sac-SMA parameters'
  
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
          case ('uztwm')
            read(readline, *, iostat=ios) this%uztwm
            n_params_read = n_params_read + 1
          case ('uzfwm')
            read(readline, *, iostat=ios) this%uzfwm
            n_params_read = n_params_read + 1
          case ('lztwm')
            read(readline, *, iostat=ios) this%lztwm
            n_params_read = n_params_read + 1
          case ('lzfpm')
            read(readline, *, iostat=ios) this%lzfpm
            n_params_read = n_params_read + 1
          case ('lzfsm')
            read(readline, *, iostat=ios) this%lzfsm
            n_params_read = n_params_read + 1
          case ('adimp')
            read(readline, *, iostat=ios) this%adimp
            n_params_read = n_params_read + 1
          case ('uzk')
            read(readline, *, iostat=ios) this%uzk
            n_params_read = n_params_read + 1
          case ('lzpk')
            read(readline, *, iostat=ios) this%lzpk
            n_params_read = n_params_read + 1
          case ('lzsk')
            read(readline, *, iostat=ios) this%lzsk
            n_params_read = n_params_read + 1
          case ('zperc')
            read(readline, *, iostat=ios) this%zperc
            n_params_read = n_params_read + 1
          case ('rexp')
            read(readline, *, iostat=ios) this%rexp
            n_params_read = n_params_read + 1
          case ('pctim')
            read(readline, *, iostat=ios) this%pctim
            n_params_read = n_params_read + 1
          case ('pfree')
            read(readline, *, iostat=ios) this%pfree
            n_params_read = n_params_read + 1
          case ('riva')
            read(readline, *, iostat=ios) this%riva
            n_params_read = n_params_read + 1
          case ('side')
            read(readline, *, iostat=ios) this%side
            n_params_read = n_params_read + 1
          case ('rserv')
            read(readline, *, iostat=ios) this%rserv
            n_params_read = n_params_read + 1
          case default
            print *, 'Parameter ',param,' not recognized in sac parameter file'
        end select
  
      end if
  
    end do
    close(unit=51)
  
    ! quick check on completeness
    if(n_params_read /= 18) then
      print *, 'Read ', n_params_read , ' Sac-SMA params, but need 18.  Quitting.'; stop
    end if
    
    ! calculate derived parameters

    this%total_area = 0.0
    do nh=1, runinfo%n_hrus
      this%total_area = this%total_area + this%hru_area(nh)
    end do
    
    return
  end subroutine read_sac_parameters

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
    integer			    :: yr, mnth, dy, hr, found_start, ios, skipcount
    real			    :: pcp, tav, pet

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
        read (UNIT=runinfo%forcing_fileunits(nh), FMT=*, IOSTAT=ierr) yr, mnth, dy, hr, pcp, tav, pet

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
    integer		                     :: nh, ierr=0
    integer		                     :: yr, mnth, dy, hr
    character(len=10)                        :: forcing_datehr
    
    !print*, "---"; print*, 'Current run datehr is ', runinfo%curr_datehr
  
    ! loop over sub-areas and read their forcings
    do nh=1, runinfo%n_hrus

      ! read one record from already open files and check success
      read (UNIT=runinfo%forcing_fileunits(nh), FMT=*, IOSTAT=ierr) yr, mnth, dy, hr, forcing%precip(nh), forcing%tair(nh), forcing%pet(nh)
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
      ! NOTE: this is written now for a single temperature input (tair), but the standard for running sac+sac is tmin/tmax
      !       we could return to the standard though a namelist option if needed
                                                             
    end do  ! end loop across hrus
    
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
    write(runinfo%output_fileunits(1),'(A)') 'year mo dy hr tair precip pet qs qg tci eta roimp sdro ssur sif bfs bfp'   ! header

    ! if user setting is to write out information for each snowband, open the individual files
    if (namelist%output_hrus == 1) then
      do nh=1, runinfo%n_hrus

        ! make filename to read
        filename = trim(namelist%output_root) // trim(parameters%hru_id(nh)) // '.txt'	

        ! Open the output files
        open(runinfo%output_fileunits(nh+1), file = trim(filename), form = 'formatted', action = 'write', status='replace', iostat = ierr)
        if (ierr /= 0) then
          write(*,'("Problem opening file ''", A, "''")') trim(filename)
          stop ":  ERROR EXIT"
        endif
      
        ! Write 1-line header
        write(runinfo%output_fileunits(nh+1),'(A)') 'year mo dy hr tair precip pet qs qg tci eta roimp sdro ssur sif bfs bfp bfncc'
        
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
        filename = trim(namelist%sac_state_out_root) // trim(parameters%hru_id(nh)) // '.txt'	

        ! Open the output files
        open(runinfo%state_fileunits(nh), file = trim(filename), form = 'formatted', action = 'write', status='replace', iostat = ierr)
        if (ierr /= 0) then
          write(*,'("Problem opening file ''", A, "''")') trim(filename)
          stop ":  ERROR EXIT"
        endif
      
        ! Write 1-line header
        write(runinfo%state_fileunits(nh),'(A)') &
         'datehr        uztwc    uzfwc   lztwc   lzfsc   lzfpc    adimc'
  
      end do  ! end loop over sub-units
    end if   
    
  END SUBROUTINE init_new_state_files
  
  ! === write state information for one timestep ===
  subroutine write_sac_statefile(runinfo, namelist, modelvar, n_curr_hru)
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
          modelvar%uztwc(n_curr_hru), modelvar%uzfwc(n_curr_hru), modelvar%lztwc(n_curr_hru), &
          modelvar%lzfsc(n_curr_hru), modelvar%lzfpc(n_curr_hru), modelvar%adimc(n_curr_hru), modelvar%bfncc(n_curr_hru)
    if(ierr /= 0) then
      print*, 'ERROR writing state file information for sub-unit ', n_curr_hru; stop
    endif
    
    return
  end subroutine write_sac_statefile
  

  ! === read state information for one timestep before start of run ===
  subroutine read_sac_statefiles (modelvar, namelist, parameters, runinfo) 
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
      state_filename = trim(namelist%sac_state_in_root) // trim(parameters%hru_id(hru)) // '.txt'
      open(unit=95,FILE=trim(state_filename), FORM='formatted', status='old')
      !print*, ' -- reading sac state file: ', trim(state_filename)
  
      ! format for input is an unknown number of rows with 20 data columns (1 tprev, 19 for cs)
      !   the first column is the datestring; neg ios means end of file; pos means something wrong

      ! skip header row
      read(95, *, IOSTAT=ios)
      
      ! read each row and check to see if the date matches the initial state date
      do while(ios .eq. 0)
  
        read(95, *, IOSTAT=ios) statefile_datehr, modelvar%uztwc(hru), modelvar%uzfwc(hru), &
            modelvar%lztwc(hru), modelvar%lzfsc(hru), modelvar%lzfpc(hru), modelvar%adimc(hru), &
            modelvar%eta(hru)
  
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
      print*, 'ERROR:  matching state not found in sac restart file.  Looking for state date: ', state_datehr
      print*, '  -- last state read was on: ', statefile_datehr
      print*, 'Stopping.  Check inputs!'; stop
    endif
    
    return
  
  end subroutine read_sac_statefiles
  

  ! === write output for one timestep ===
  ! assumes that files have been opened and header already written
  SUBROUTINE write_sac_output(namelist, runinfo, parameters, forcing, modelvar, derived, n_curr_hru)
    implicit none
    type (namelist_type),    intent(in)     :: namelist
    type (runinfo_type),     intent(in)     :: runinfo
    type (parameters_type),  intent(in)     :: parameters
    integer, intent(in)                     :: n_curr_hru   ! number of the current hru being simulated
    
    ! forcing & modelvar are because the combined variables get updated here
    type (forcing_type),     intent(inout)  :: forcing
    type (modelvar_type),    intent(inout)  :: modelvar
    type (derived_type),     intent(inout)  :: derived
    
    ! local variables
    integer        :: ierr   ! error code returned by open(iostat = ierr)
    integer        :: nh     ! loop counter
  
    ! ==== WRITE output for current area simulation ====
    ! Note:  write order should match header written by open_and_init_output_files()
    
    32 FORMAT(I4.4, 3(1x,I2.2), 20(F10.3))

    ! if user setting is to write out information for each snowband, open the individual files
    if (namelist%output_hrus == 1 .and. runinfo%n_hrus > 1) then
      write(runinfo%output_fileunits(n_curr_hru+1), 32, iostat=ierr) runinfo%curr_yr, runinfo%curr_mo, runinfo%curr_dy, runinfo%curr_hr, &
            forcing%tair(n_curr_hru), forcing%precip(n_curr_hru), forcing%pet(n_curr_hru), &
            modelvar%qs(n_curr_hru), modelvar%qg(n_curr_hru), modelvar%tci(n_curr_hru), & 
            modelvar%eta(n_curr_hru), modelvar%roimp(n_curr_hru), modelvar%sdro(n_curr_hru), &
            modelvar%ssur(n_curr_hru), modelvar%sif(n_curr_hru), modelvar%bfs(n_curr_hru), &
            modelvar%bfp(n_curr_hru), modelvar%bfncc(n_curr_hru)
      if(ierr /= 0) then
        print*, 'ERROR writing output information for basin average'; stop
      endif            
    end if  ! IF case for writing HRU-specific output to file (not including states)

    ! ==== if all hrus have been run, sum across hrus with weighting for snowband area ====
    
    ! initialize for timestep
    derived%tair_comb       = 0.0
    derived%precip_comb     = 0.0
    derived%pet_comb        = 0.0
    derived%qs_comb        = 0.0
    derived%qg_comb        = 0.0
    derived%tci_comb       = 0.0
    derived%eta_comb       = 0.0
    derived%roimp_comb     = 0.0
    derived%sdro_comb      = 0.0
    derived%ssur_comb      = 0.0
    derived%sif_comb       = 0.0
    derived%bfs_comb       = 0.0
    derived%bfp_comb       = 0.0
        
    if (n_curr_hru .eq. runinfo%n_hrus) then 
      do nh=1, runinfo%n_hrus
        derived%tair_comb        = derived%tair_comb + forcing%tair(nh) * parameters%hru_area(nh)
        derived%precip_comb      = derived%precip_comb + forcing%precip(nh) * parameters%hru_area(nh)
        derived%pet_comb         = derived%pet_comb + forcing%pet(nh) * parameters%hru_area(nh)
        derived%qs_comb          = derived%qs_comb + modelvar%qs(nh) * parameters%hru_area(nh) 
        derived%qg_comb          = derived%qg_comb + modelvar%qg(nh) * parameters%hru_area(nh) 
        derived%tci_comb         = derived%tci_comb + modelvar%tci(nh) * parameters%hru_area(nh)
        derived%eta_comb         = derived%eta_comb + modelvar%eta(nh) * parameters%hru_area(nh)
        derived%roimp_comb       = derived%roimp_comb + modelvar%roimp(nh) * parameters%hru_area(nh)
        derived%sdro_comb        = derived%sdro_comb  + modelvar%sdro(nh) * parameters%hru_area(nh)
        derived%ssur_comb        = derived%ssur_comb  + modelvar%ssur(nh) * parameters%hru_area(nh)
        derived%sif_comb         = derived%sif_comb + modelvar%sif(nh) * parameters%hru_area(nh)
        derived%bfs_comb         = derived%bfs_comb + modelvar%bfs(nh) * parameters%hru_area(nh)
        derived%bfp_comb         = derived%bfp_comb + modelvar%bfp(nh) * parameters%hru_area(nh)
      end do

      ! take average of weighted sum of HRU areas
      derived%tair_comb        = derived%tair_comb / parameters%total_area
      derived%precip_comb      = derived%precip_comb / parameters%total_area
      derived%pet_comb         = derived%pet_comb / parameters%total_area
      derived%qs_comb          = derived%qs_comb / parameters%total_area
      derived%qg_comb          = derived%qg_comb / parameters%total_area
      derived%tci_comb         = derived%tci_comb / parameters%total_area
      derived%eta_comb         = derived%eta_comb / parameters%total_area
      derived%roimp_comb       = derived%roimp_comb / parameters%total_area
      derived%sdro_comb        = derived%sdro_comb / parameters%total_area
      derived%ssur_comb        = derived%ssur_comb / parameters%total_area
      derived%sif_comb         = derived%sif_comb / parameters%total_area
      derived%bfs_comb         = derived%bfs_comb / parameters%total_area
      derived%bfp_comb         = derived%bfp_comb / parameters%total_area

      ! -- write out combined file that is similar to each area file, but add flow variable in CFS units
      write(runinfo%output_fileunits(1), 32, iostat=ierr) runinfo%curr_yr, runinfo%curr_mo, runinfo%curr_dy, runinfo%curr_hr, &
            derived%tair_comb, derived%precip_comb, derived%pet_comb, &
            derived%qs_comb, derived%qg_comb, derived%tci_comb,derived%eta_comb, &
            derived%roimp_comb, derived%sdro_comb, derived%ssur_comb, &
            derived%sif_comb, derived%bfs_comb, derived%bfp_comb
      if(ierr /= 0) then
        print*, 'ERROR writing output information for sub-unit ', n_curr_hru; stop
      endif
    endif 

    return
  END SUBROUTINE write_sac_output

  
end module ioModule

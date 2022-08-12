! ====================================================================================
! Driver for the Snow17 model for multiple HRUs within a basin and writes zone and areal mean output
!
! Adapted by A. Wood (2022) from the NCAR Snow17SacUH codebase (https://github.com/NCAR/NWS_hydro_models) 
!   that was formulated from Office of Hydrology FORTRAN source files obtained circa 2012-2013
! 
! Feb 2022:  Reformulated to use BMI at driver level with compiler options for running in NextGen framework
!
! ====================================================================================

program multi_driver
  
  !---------------------------------------------------------------------
  !  Modules
  !  Only the BMI modules need to be exposed
  !---------------------------------------------------------------------
  use bmi_snow17_module
  use bmif_2_0
  use dateTimeUtilsModule

  implicit none

  !---------------------------------------------------------------------
  !  Types (only the bmi_snow17 type needed)
  !---------------------------------------------------------------------
  type (bmi_snow17)  :: m

  !---------------------------------------------------------------------
  !  Local variable(s) 
  !---------------------------------------------------------------------
  character (len = 400) :: namelist_file    ! command line argument for namelist file
  double precision      :: current_time     ! current time coordinate of model in s (unixtime)
  double precision      :: end_time         ! end of model simulation period in s (unixtime)
  double precision      :: dt               ! length of timestep in s (unixtime)
  ! note, these times may need to change to time relative to some other start
  integer               :: status           ! returns status values (function return codes)
  character(len=10)     :: start_datehr      ! Start date of the model run ( YYYYMMDDHH ) 
  character(len=10)     :: end_datehr        ! End date of the model run ( YYYYMMDDHH ) 
  character(len=10)     :: curr_datehr       ! Current date of the model run ( YYYYMMDDHH ) 

  !---------------------------------------------------------------------
  !  Initialize
  !  Call the initialize_from_file() subroutine in src/RunSnow17.f90
  !---------------------------------------------------------------------
  print*, "Initializing run ..."
  call get_command_argument(1, namelist_file, status=status)
  !if( .not. ( present(namelist_file) ) ) then
  if( status /= 0 ) then
    namelist_file = "namelist.input"
    print*, 'No namelist filename supplied -- attempting to read default file called namelist.input'
  endif  
  status = m%initialize(namelist_file)

  !---------------------------------------------------------------------
  ! Run the model
  ! m%update() calls advance_in_time() subroutine in ../src/RunSnow17.f90
  !---------------------------------------------------------------------
  status = m%get_current_time(current_time)
  status = m%get_end_time(end_time)
  status = m%get_time_step(dt)
  
  ! create optional screen output for run times
  call unix_to_datehr(current_time, start_datehr) 
  call unix_to_datehr(end_time, end_datehr) 
  print*,'----';
  print*, 'Running model => start: ', start_datehr, ' end: ', end_datehr, ' timesteps: ', int((end_time - current_time)/dt)
  print*,'----'
  
  ! loop through while current time <= end time (
  do while (current_time .le. end_time)
    !print*, 'Current time:', current_time
    status = m%update()                       ! run the model one time step
    status = m%get_current_time(current_time) ! update current_time
  end do

  !---------------------------------------------------------------------
  ! Finalize the model run
  ! All model finalization code in ../src/RunSnow17.f90
  !---------------------------------------------------------------------
  print*, "Finalizing..."
  status = m%finalize()
  print*, "DONE"

end program multi_driver

module runInfoType

use namelistModule, only: namelist_type
use dateTimeUtilsModule

implicit none
save
!private

type, public :: runinfo_type

  ! time-space information about the model run
  integer                             :: n_hrus		       ! number of HRU areas in parameter files
  character(len=10)                   :: start_datehr      ! Start date of the model run ( YYYYMMDDHH ) 
  character(len=10)                   :: end_datehr        ! End date of the model run ( YYYYMMDDHH ) 
  character(len=10)                   :: curr_datehr       ! Current date of the model run ( YYYYMMDDHH ) 
  
  double precision                    :: start_datetime    ! unix start datetime (s since 1970-01-01 00:00:00) ?UTC? 
  double precision                    :: end_datetime      ! unix end datetime (s since 1970-01-01 00:00:00) ?UTC? 
  double precision                    :: curr_datetime     ! unix current datetime (s since 1970-01-01 00:00:00) ?UTC? 
  double precision                    :: time_dbl          ! current time of model run in seconds from beginning
  double precision                    :: dt                ! run timestep (s)
  integer                             :: itime             ! current integer time step of model run
  integer                             :: ntimes            ! total number of time steps in model run
  ! note: previously used 'real*8' instead of double precision
  
  integer                             :: curr_yr, curr_mo, curr_dy, curr_hr, curr_min, curr_sec  
  integer                             :: start_year, start_month, start_day, start_hour
  integer                             :: end_year, end_month, end_day, end_hour
  integer, dimension(:), allocatable  :: forcing_fileunits  ! fileunit numbers for multi-hru forcings
  integer, dimension(:), allocatable  :: output_fileunits   ! fileunit numbers for multi-hru outputs
  integer, dimension(:), allocatable  :: state_fileunits    ! fileunit numbers for multi-hru state files

  contains

    procedure, public  :: initInfo

end type runinfo_type

contains   

  subroutine initInfo(this, namelist)
  
    use defNamelist
    implicit none

    class(runinfo_type),intent(inout) :: this
    type(namelist_type),intent(in)    :: namelist
    
    ! local vars
    integer                           :: nh

    ! variable allocations
    allocate(this%forcing_fileunits(namelist%n_hrus))
    allocate(this%state_fileunits(namelist%n_hrus))
    allocate(this%output_fileunits(namelist%n_hrus+1))   ! extra file-unit for combined basin outputs
   
    ! namelist-based assignments
    this%dt           = namelist%model_timestep         ! in seconds
    this%start_datehr = namelist%start_datehr
    this%end_datehr   = namelist%end_datehr
    this%n_hrus       = namelist%n_hrus
    
    ! calculated / derived variables
    read(this%start_datehr(1:4), '(I4)') this%start_year   ! converting char dates to int date elements
    read(this%start_datehr(5:6), '(I4)') this%start_month
    read(this%start_datehr(7:8), '(I4)') this%start_day
    read(this%start_datehr(9:10), '(I4)') this%start_hour
    read(this%end_datehr(1:4), '(I4)') this%end_year
    read(this%end_datehr(5:6), '(I4)') this%end_month
    read(this%end_datehr(7:8), '(I4)') this%end_day
    read(this%end_datehr(9:10), '(I4)') this%end_hour
    this%curr_yr      = this%start_year
    this%curr_mo      = this%start_month
    this%curr_dy      = this%start_day
    this%curr_hr      = this%start_hour
    this%curr_min     = 0
    this%curr_sec     = 0
    this%curr_datehr  = this%start_datehr    ! default setting needed to advance the forcings to the start date
    
    ! unix times
    this%start_datetime = date_to_unix(namelist%start_datehr)  ! returns seconds-since-1970-01-01 00
    this%end_datetime   = date_to_unix(namelist%end_datehr)    
    this%curr_datetime  = this%start_datetime
    this%ntimes         = int( (this%start_datetime - this%end_datetime)/this%dt + 1)   ! inclusive of last timestep

    ! other default assignments
    this%itime          = 1                    ! initialize the time loop counter at 1
    this%time_dbl       = 0.d0                 ! start model run at t = 0.0  
    
    ! assign input forcing and output fileunit numbers
    do nh = 1, n_hrus
      this%forcing_fileunits(nh) = 250 + nh     
      this%output_fileunits(nh)  = 500 + nh       ! allows for 250 non-overlapping fileunits
      this%state_fileunits(nh)   = 750 + nh       ! allows for 250 non-overlapping fileunits
    end do
    this%output_fileunits(nh)    = 500 + nh + 1   ! add one more unit for combined ouptuts

  end subroutine initInfo

end module runInfoType

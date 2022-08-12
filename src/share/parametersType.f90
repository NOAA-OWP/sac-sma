module parametersType

use namelistModule, only: namelist_type

implicit none
save

! -- variable declarations

type, public :: parameters_type

  ! Snow17 model params in the snow param file
  character(len = 20), dimension(:), allocatable :: hru_id     ! local hru ids for multiple hrus
  real, dimension(:), allocatable                :: hru_area   ! sq-km, needed for combination & routing conv.
  real, dimension(:), allocatable                :: latitude   ! centroid latitude of hru (decimal degrees)
  real, dimension(:), allocatable                :: elev       ! mean elevation of hru (m)
  real, dimension(:), allocatable                :: scf, mfmax, mfmin, uadj, si, pxtemp
  real, dimension(:), allocatable                :: nmf, tipm, mbase, plwhc, daygm
  real, dimension(:,:), allocatable              :: adc        ! snow depletion curve (hrus, ordinates)
  
  ! derived vars
  real                                           :: total_area  ! total basin area used in averaging outputs

  contains

    procedure, public  :: initParams

end type parameters_type

contains

  subroutine initParams(this, namelist)
  
    use defNamelist

    class(parameters_type), intent(inout)    :: this
    type(namelist_type), intent(in)          :: namelist
    
    ! allocate variables
    allocate(this%hru_id(n_hrus))
    allocate(this%hru_area(n_hrus))
    allocate(this%latitude(n_hrus))
    allocate(this%elev(n_hrus))
    allocate(this%scf(n_hrus))
    allocate(this%mfmax(n_hrus))
    allocate(this%mfmin(n_hrus))
    allocate(this%uadj(n_hrus))
    allocate(this%si(n_hrus))
    allocate(this%pxtemp(n_hrus))
    allocate(this%nmf(n_hrus))
    allocate(this%tipm(n_hrus))
    allocate(this%mbase(n_hrus))
    allocate(this%plwhc(n_hrus))
    allocate(this%daygm(n_hrus))    

    !
    !Reversed the row and column for this `adc' array such that
    ! we can pass a slice of the array to other subroutines in
    ! a contiguous memory. Otherwise, we will receive warnings
    ! at runtime about creation of a temporary array and the performance
    ! is impaired. The reason is that
    ! Fortran stores arrays as 'column major'. 
    !
    !allocate(this%adc(n_hrus, 11)) !=> runtime warning    ! 11 points (0.0 to 1.0 in 0.1 increments)
    !
    allocate(this%adc(11,n_hrus))    ! 11 points (0.0 to 1.0 in 0.1 increments)
    
    ! assign defaults (if any)
    this%total_area  = huge(1.0)
    
  end subroutine initParams

end module parametersType

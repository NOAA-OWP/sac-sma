module parametersType

use namelistModule, only: namelist_type

implicit none
save

! -- variable declarations

type, public :: parameters_type

  ! Sac model params in the sac param file
  character(len = 20), dimension(:), allocatable :: hru_id     ! local hru ids for multiple hrus
  DOUBLE PRECISION, dimension(:), allocatable                :: hru_area   ! sq-km, needed for combination & routing conv.
  DOUBLE PRECISION, dimension(:), allocatable                :: uztwm, uzfwm, lztwm, lzfsm, lzfpm, adimp
  DOUBLE PRECISION, dimension(:), allocatable                :: uzk, lzpk, lzsk, zperc, rexp
  DOUBLE PRECISION, dimension(:), allocatable                :: pctim, pfree, riva, side, rserv
  ! derived vars
  DOUBLE PRECISION                                           :: total_area  ! total basin area used in averaging outputs

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
    allocate(this%uztwm(n_hrus))
    allocate(this%uzfwm(n_hrus))
    allocate(this%lztwm(n_hrus))
    allocate(this%lzfsm(n_hrus))
    allocate(this%lzfpm(n_hrus))
    allocate(this%adimp(n_hrus))
    allocate(this%uzk(n_hrus))
    allocate(this%lzpk(n_hrus))
    allocate(this%lzsk(n_hrus))
    allocate(this%zperc(n_hrus))
    allocate(this%rexp(n_hrus))
    allocate(this%pctim(n_hrus))
    allocate(this%pfree(n_hrus))
    allocate(this%riva(n_hrus))
    allocate(this%side(n_hrus))
    allocate(this%rserv(n_hrus))    

    
    ! assign defaults (if any)
    this%total_area  = huge(1.0)
    
  end subroutine initParams

end module parametersType

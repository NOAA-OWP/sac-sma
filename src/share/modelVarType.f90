module modelVarType

  use namelistModule, only: namelist_type

  implicit none
  save

  type, public :: modelvar_type

    ! main model states and flux variable
    DOUBLE PRECISION, dimension(:), allocatable    :: uztwc  ! Upper zone tension water storage content (mm) 
    DOUBLE PRECISION, dimension(:), allocatable    :: uzfwc  ! Upper zone free water storage content (mm)
    DOUBLE PRECISION, dimension(:), allocatable    :: lztwc  ! Lower zone tension water storage content (mm)
    DOUBLE PRECISION, dimension(:), allocatable    :: lzfsc  ! Lower zone free secondary water storage content (mm)
    DOUBLE PRECISION, dimension(:), allocatable    :: lzfpc  ! Lower zone free primary water storage content (mm)
    DOUBLE PRECISION, dimension(:), allocatable    :: adimc  ! Additional impervious area content (mm)
    DOUBLE PRECISION, dimension(:), allocatable    :: qs     ! surface runoff from all sources (mm)    
    DOUBLE PRECISION, dimension(:), allocatable    :: qg     ! baseflow (mm)
    DOUBLE PRECISION, dimension(:), allocatable    :: tci    ! total channel inflow (mm)    
    DOUBLE PRECISION, dimension(:), allocatable    :: eta    ! actual evapotranspiration (mm) 
    DOUBLE PRECISION, dimension(:), allocatable    :: roimp  ! impervious area runoff (mm)
    DOUBLE PRECISION, dimension(:), allocatable    :: sdro   ! direct runoff (mm)
    DOUBLE PRECISION, dimension(:), allocatable    :: ssur   ! surface runoff (mm)
    DOUBLE PRECISION, dimension(:), allocatable    :: sif    ! interflow (mm)
    DOUBLE PRECISION, dimension(:), allocatable    :: bfs    ! channel baseflow component (mm)
    DOUBLE PRECISION, dimension(:), allocatable    :: bfp    ! channel baseflow component (mm)
    DOUBLE PRECISION, dimension(:), allocatable    :: bfncc  ! baseflow non-channelcomponent (mm)
            
    contains

      procedure, public  :: initModelVar

  end type modelvar_type

  contains   

  subroutine initModelVar(this, namelist)
  
    implicit none

    ! define variables
    class(modelvar_type), intent(out) :: this
    type(namelist_type), intent(in)   :: namelist
    
    ! -- variable allocations (time dim not needed since forcings are one-rec scalars)
    allocate(this%uztwc (1:namelist%n_hrus))
    allocate(this%uzfwc (1:namelist%n_hrus))
    allocate(this%lztwc (1:namelist%n_hrus))
    allocate(this%lzfsc (1:namelist%n_hrus))
    allocate(this%lzfpc (1:namelist%n_hrus))
    allocate(this%adimc (1:namelist%n_hrus))
    allocate(this%qs    (1:namelist%n_hrus))
    allocate(this%qg    (1:namelist%n_hrus))
    allocate(this%tci   (1:namelist%n_hrus))
    allocate(this%eta   (1:namelist%n_hrus)) 
    allocate(this%roimp (1:namelist%n_hrus))
    allocate(this%sdro  (1:namelist%n_hrus))
    allocate(this%ssur  (1:namelist%n_hrus))
    allocate(this%sif   (1:namelist%n_hrus))
    allocate(this%bfs   (1:namelist%n_hrus))
    allocate(this%bfp   (1:namelist%n_hrus))
    allocate(this%bfncc (1:namelist%n_hrus))   
! -- default assignmtents
    this%uztwc(:)      = 0.0
    this%uzfwc(:)      = 0.0 
    this%lztwc(:)      = 0.0 
    this%lzfsc(:)      = 0.0 
    this%lzfpc(:)      = 0.0
    this%adimc(:)      = 0.0
    this%qs(:)         = 0.0
    this%qg(:)         = 0.0
    this%tci(:)        = 0.0
    this%eta(:)        = 0.0
    this%roimp(:)      = 0.0
    this%sdro(:)       = 0.0
    this%ssur(:)       = 0.0
    this%sif(:)        = 0.0
    this%bfs(:)        = 0.0
    this%bfp(:)        = 0.0
    this%bfncc(:)      = 0.0
  end subroutine initModelVar

end module modelVarType

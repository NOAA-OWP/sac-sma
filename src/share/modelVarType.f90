module modelVarType

  use namelistModule, only: namelist_type

  implicit none
  save

  type, public :: modelvar_type

    ! main model states and flux variable
    real, dimension(:), allocatable    :: uztwc  ! Upper zone tension water storage content (mm) 
    real, dimension(:), allocatable    :: uzfwc  ! Upper zone free water storage content (mm)
    real, dimension(:), allocatable    :: lztwc  ! Lower zone tension water storage content (mm)
    real, dimension(:), allocatable    :: lzfsc  ! Lower zone free secondary water storage content (mm)
    real, dimension(:), allocatable    :: lzfpc  ! Lower zone free primary water storage content (mm)
    real, dimension(:), allocatable    :: adimc  ! Additional impervious area content (mm)
  
    ! other states and carryover variables
    !real, dimension(:), allocatable    :: tprev
      
    ! areally-averaged variables for output 
    !real                               :: sneqv_comb, snowh_comb, raim_comb
  
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
    !
    !Reversed the row and column for this `cs' array such that
    ! we can pass a slice of the array to other subroutines in
    ! a contiguous memory. Otherwise, we will receive warnings
    ! at runtime about creation of a temporary array. and the performance
    ! is impaired. The reason is that
    ! Fortran stores arrays as 'column major'. 
    !
    !!allocate(this%cs    (1:namelist%n_hrus, 1:19)) => runtime warnings
    !
    !allocate(this%cs    (1:19, 1:namelist%n_hrus))
    
    ! -- default assignments
    this%uztwc(:)      = 0.0
    this%uzfwc(:)      = 0.0 
    this%lztwc(:)      = 0.0 
    this%lzfsc(:)      = 0.0 
    this%lzfpc(:)      = 0.0
    this%adimc(:)      = 0.0
     
    !this%tprev(:)      = 0.0      ! prev. temp is needed
    !this%cs(:,:)       = 0.0      ! prev. temp is needed
    
    ! -- estimate derived variables (if any)

  end subroutine initModelVar

end module modelVarType

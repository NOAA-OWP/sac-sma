module modelVarType

  use namelistModule, only: namelist_type

  implicit none
  save

  type, public :: modelvar_type

    ! main model states and flux variable
    real, dimension(:), allocatable    :: sneqv  ! snow water equivalent (unit)
    real, dimension(:), allocatable    :: snowh  ! snow height
    real, dimension(:), allocatable    :: snow   ! snow (?) -- seems same as snowh but used in snow19()
    real, dimension(:), allocatable    :: raim   ! rain and melt output
  
    ! other states and carryover variables
    real, dimension(:), allocatable    :: tprev
    real, dimension(:,:), allocatable  :: cs     ! 19-element vector per HRU used in snow19: (n_hrus, 19)
  
    ! areally-averaged variables for output 
    real                               :: sneqv_comb, snowh_comb, raim_comb
  
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
    allocate(this%raim  (1:namelist%n_hrus))
    allocate(this%sneqv (1:namelist%n_hrus))
    allocate(this%snowh (1:namelist%n_hrus))
    allocate(this%snow  (1:namelist%n_hrus))
    allocate(this%tprev (1:namelist%n_hrus))
    !
    !Reversed the row and column for this `cs' array such that
    ! we can pass a slice of the array to other subroutines in
    ! a contiguous memory. Otherwise, we will receive warnings
    ! at runtime about creation of a temporary array. and the performance
    ! is impaired. The reason is that
    ! Fortran stores arrays as 'column major'. 
    !
    !allocate(this%cs    (1:namelist%n_hrus, 1:19)) => runtime warnings
    !
    allocate(this%cs    (1:19, 1:namelist%n_hrus))
    
    ! -- default assignments
    this%raim(:)       = 0.0
    this%sneqv(:)      = 0.0 
    this%snowh(:)      = 0.0 
    this%snow(:)       = 0.0 
    this%sneqv_comb    = 0.0
    this%snowh_comb    = 0.0
    this%raim_comb     = 0.0 
    this%tprev(:)      = 0.0      ! prev. temp is needed
    this%cs(:,:)       = 0.0      ! prev. temp is needed
    
    ! -- estimate derived variables (if any)

  end subroutine initModelVar

end module modelVarType

module forcingType

use namelistModule, only: namelist_type

implicit none
save
!private

type, public :: forcing_type

  ! atmospheric inputs & outputs (surface meteorology)
  real, dimension(:), allocatable   :: tair       ! surface air temperature [K]
  real, dimension(:), allocatable   :: precip     ! total input precipitation [mm/s]
  real, dimension(:), allocatable   :: pet        ! potential evapotranspiration [mm]
  real                              :: precip_comb, pet_comb, tair_comb    ! areally averaged forcings across HRUs

  contains

    procedure, public   :: initForcing

end type forcing_type

contains   

  subroutine initForcing(this, namelist)

    use defNamelist
    implicit none

    class(forcing_type), intent(out) :: this
    type(namelist_type), intent(in)  :: namelist

    ! -- variable allocations (time dim not needed since forcings are one-rec scalars)
    allocate(this%tair       (n_hrus))
    allocate(this%precip     (n_hrus))
    allocate(this%pet        (n_hrus))
    
    ! -- default assignments
    this%precip(:)         = huge(1.0)
    this%pet       (:)     = huge(1.0)
    this%tair(:)           = huge(1.0)
    this%precip_comb       = 0.0
    this%tair_comb         = 0.0
    this%pet_comb          = 0.0
    
  end subroutine initForcing
  

end module forcingType

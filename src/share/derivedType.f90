module derivedType

  use namelistModule, only: namelist_type

  implicit none
  save

  type, public :: derived_type
     
    ! areally-averaged variables for output 
    real                               :: qs_comb, qg_comb, tci_comb, eta_comb
    real                               :: roimp_comb, sdro_comb, ssur_comb
    real                               :: sif_comb, bfs_comb, bfp_comb
    real                               :: precip_comb, tair_comb, pet_comb
     
    contains

      procedure, public  :: initDerived

  end type derived_type

  contains   

  subroutine initDerived(this, namelist)
  
    implicit none

    ! define variables
    class(derived_type), intent(out) :: this
    type(namelist_type), intent(in)   :: namelist
      
! -- default assignments
    this%qs_comb       = 0.0
    this%qg_comb       = 0.0
    this%tci_comb      = 0.0 
    this%eta_comb      = 0.0
    this%roimp_comb    = 0.0
    this%sdro_comb     = 0.0
    this%ssur_comb     = 0.0
    this%sif_comb      = 0.0
    this%bfs_comb      = 0.0
    this%bfp_comb      = 0.0
    this%precip_comb   = 0.0
    this%tair_comb     = 0.0
    this%pet_comb      = 0.0
  end subroutine initDerived

end module derivedType

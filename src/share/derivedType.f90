module derivedType

  use namelistModule, only: namelist_type

  implicit none
  save

  type, public :: derived_type
    !areally-average variables for output
    real                               :: qs_comb, qg_comb, tci_comb, eta_comb 
    real                               :: roimp_comb, sdro_comb, ssur_comb    
    real                               :: sif_comb, bfs_comb, bfp_comb        
    real                               :: precip_comb, tair_comb, pet_comb    
    !variables for the mass balance check
    real, dimension(:), allocatable    :: precip_sum, eta_sum,tci_sum 
    real, dimension(:), allocatable    :: delta_uztwc_sum, delta_uzfwc_sum     
    real, dimension(:), allocatable    :: delta_lztwc_sum, delta_lzfsc_sum     
    real, dimension(:), allocatable    :: delta_lzfpc_sum, delta_adimc_sum     
    real, dimension(:), allocatable    :: delta_storage_sum, mass_balance       
    real, dimension(:), allocatable    :: bfncc_sum
    contains

      procedure, public  :: initDerived

  end type derived_type

  contains   

  subroutine initDerived(this, namelist)
  
    implicit none

    ! define variables
    class(derived_type), intent(out) :: this
    type(namelist_type), intent(in)   :: namelist

    ! -- variable allocations (time dim not needed since forcings are one-rec scalars)
    allocate(this%precip_sum (1:namelist%n_hrus))
    allocate(this%eta_sum    (1:namelist%n_hrus))
    allocate(this%tci_sum    (1:namelist%n_hrus))
    allocate(this%delta_uztwc_sum (1:namelist%n_hrus))
    allocate(this%delta_uzfwc_sum (1:namelist%n_hrus))
    allocate(this%delta_lztwc_sum (1:namelist%n_hrus))
    allocate(this%delta_lzfsc_sum (1:namelist%n_hrus))
    allocate(this%delta_lzfpc_sum (1:namelist%n_hrus))
    allocate(this%delta_adimc_sum (1:namelist%n_hrus))
    allocate(this%delta_storage_sum (1:namelist%n_hrus))
    allocate(this%mass_balance    (1:namelist%n_hrus))
    allocate(this%bfncc_sum       (1:namelist%n_hrus))
      
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
    this%precip_sum    = 0.0
    this%eta_sum       = 0.0
    this%tci_sum       = 0.0
    this%delta_uztwc_sum = 0.0
    this%delta_uzfwc_sum = 0.0
    this%delta_lztwc_sum = 0.0
    this%delta_lzfsc_sum = 0.0
    this%delta_lzfpc_sum = 0.0
    this%delta_adimc_sum = 0.0
    this%delta_storage_sum = 0.0
    this%mass_balance    = 0.0
    this%bfncc_sum       = 0.0
  end subroutine initDerived

end module derivedType

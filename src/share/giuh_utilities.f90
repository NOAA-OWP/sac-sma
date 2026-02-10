! module for giuh calculations
module giuhModule
  use sac_log_module
  use iso_fortran_env

  implicit none
   
contains
! We need to have a giuh_ordinates array for each HRU. The number of ordinates vary by catchment size.
! use giuh_ordinates=0.06,0.51,0.28,0.12,0.03

function giuh_convolution_integral(runoff_m, giuh_ordinates, &
                                   runoff_queue_m_per_timestep) &
                                   result(runoff_m_now)

  implicit none
  integer :: sac_log_level = LOG_LEVEL_INFO

  !Function parameters
  real(kind=8), intent(in)    :: runoff_m
  real(kind=8), intent(in)    :: giuh_ordinates(:)
  real(kind=8), intent(inout) :: runoff_queue_m_per_timestep(:)

  real(kind=8) :: runoff_m_now ! Result

  integer :: i, N

  N = size(giuh_ordinates)

  ! Sanity check
  if (size(runoff_queue_m_per_timestep) < N+1) then
     error stop "runoff_queue_m_per_timestep must be size N+1"
     call write_log("Runoff_queue_m_per_timestep must be size N+1", LOG_LEVEL_FATAL)
  end if

  ! Clear temporary slot (index N+1 in Fortran)
  runoff_queue_m_per_timestep(N+1) = 0.0d0

  ! Convolution accumulation
  do i = 1, N
     runoff_queue_m_per_timestep(i) = runoff_queue_m_per_timestep(i) + &
                                      giuh_ordinates(i) * runoff_m
  end do

  ! Current runoff output
  runoff_m_now = runoff_queue_m_per_timestep(1)

  ! Shift queue left
  do i = 2, N
     runoff_queue_m_per_timestep(i-1) = runoff_queue_m_per_timestep(i)
  end do

  ! Clear last active slot
  runoff_queue_m_per_timestep(N) = 0.0d0

end function giuh_convolution_integral

end module giuhModule
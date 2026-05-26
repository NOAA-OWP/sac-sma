!------------------------------------------------------------------------------
! sac_log_module.f90
!
! Compatibility wrapper for legacy Sac-SMA logging.
!
! Purpose:
!   Many legacy Fortran components do:
!       use sac_log_module
!       call write_log(level, message)
!
!   This module keeps that interface stable while forwarding to the EWTS
!   Fortran runtime logger (module: logger). The EWTS runtime decides whether
!   to write to standalone log files or route through the NGEN bridge,
!   depending on build/run environment.
!
!------------------------------------------------------------------------------

module sac_log_module
  use logger, only: ewts_write_log_module => write_log_module, &
                    ewts_is_logger_enabled_module => is_logger_enabled_module, &
                    ewts_get_log_level_module => get_log_level_module, &
                    ewts_logger_init_module => logger_init_module
  use ewts_module_constants, only: EWTS_ID_SAC_SMA
  implicit none
  private

  integer, parameter, public :: NOTSET            = 0
  integer, parameter, public :: LOG_LEVEL_DEBUG   = 10
  integer, parameter, public :: LOG_LEVEL_PERFORM = 15
  integer, parameter, public :: LOG_LEVEL_INFO    = 20
  integer, parameter, public :: LOG_LEVEL_WARNING = 30
  integer, parameter, public :: LOG_LEVEL_SEVERE  = 40
  integer, parameter, public :: LOG_LEVEL_FATAL   = 50

  public :: write_log
  public :: is_logger_enabled, get_log_level
  public :: itoa, rtoa

  logical, save :: did_init = .false.

contains

  subroutine ensure_init()
    if (.not. did_init) then
      call ewts_logger_init_module(EWTS_ID_SAC_SMA)
      did_init = .true.
    end if
  end subroutine ensure_init

  subroutine write_log(message, level)
    character(len=*), intent(in) :: message
    integer, intent(in) :: level

    call ensure_init()
    call ewts_write_log_module(EWTS_ID_SAC_SMA, trim(message), map_level(level))
  end subroutine write_log

  logical function is_logger_enabled()
    call ensure_init()
    is_logger_enabled = ewts_is_logger_enabled_module(EWTS_ID_SAC_SMA)
  end function is_logger_enabled

  integer function get_log_level()
    call ensure_init()
    get_log_level = ewts_get_log_level_module(EWTS_ID_SAC_SMA)
  end function get_log_level

  pure integer function map_level(level) result(out_level)
    integer, intent(in) :: level

    select case (level)
      case (NOTSET, LOG_LEVEL_DEBUG, LOG_LEVEL_PERFORM, LOG_LEVEL_INFO, &
            LOG_LEVEL_WARNING, LOG_LEVEL_SEVERE, LOG_LEVEL_FATAL)
        out_level = level
      case (1)
        out_level = LOG_LEVEL_DEBUG
      case (2)
        out_level = LOG_LEVEL_PERFORM
      case (3)
        out_level = LOG_LEVEL_INFO
      case (4)
        out_level = LOG_LEVEL_WARNING
      case (5)
        out_level = LOG_LEVEL_SEVERE
      case (6)
        out_level = LOG_LEVEL_FATAL
      case default
        out_level = LOG_LEVEL_INFO
    end select
  end function map_level

  function itoa(i) result(res)
    character(:), allocatable :: res
    integer, intent(in) :: i
    character(range(i)+2) :: tmp

    write(tmp,'(i0)') i
    res = trim(tmp)
  end function itoa

  function rtoa(i) result(res)
    real, intent(in) :: i
    character(32) :: buffer
    character(len=32) :: res

    write(buffer, '(F10.10)') i
    res = adjustl(trim(buffer))
  end function rtoa
end module sac_log_module

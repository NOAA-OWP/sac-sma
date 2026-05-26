!------------------------------------------------------------------------------
! sac_log_module.f90
!
! Compatibility wrapper for legacy Sac-SMA logging.
!
! Purpose:
!   Many legacy Fortran components do:
!       use sac_log_module
!       call write_log(message, level)
!
!   This module keeps that interface stable while forwarding to the EWTS
!   Fortran runtime logger (module: logger). The EWTS runtime decides whether
!   to write to standalone log files or route through the NGEN bridge,
!   depending on build/run environment.
!
!------------------------------------------------------------------------------

module sac_log_module

#ifdef SACSMA_USE_EWTS
  use logger, only: ewts_write_log_module => write_log_module, &
                    ewts_is_logger_enabled_module => is_logger_enabled_module, &
                    ewts_get_log_level_module => get_log_level_module, &
                    ewts_logger_init_module => logger_init_module
  use ewts_module_constants, only: EWTS_ID_SAC_SMA
#else
  use, intrinsic :: iso_c_binding, only: c_long, c_int, c_ptr, c_null_ptr, c_f_pointer, c_associated
#endif

  implicit none
  private

  integer, parameter, public :: NOTSET            = 0
  integer, parameter, public :: LOG_LEVEL_DEBUG   = 10
  integer, parameter, public :: LOG_LEVEL_PERFORM = 15
  integer, parameter, public :: LOG_LEVEL_INFO    = 20
  integer, parameter, public :: LOG_LEVEL_WARNING = 30
  integer, parameter, public :: LOG_LEVEL_SEVERE  = 40
  integer, parameter, public :: LOG_LEVEL_FATAL   = 50

#ifndef SACSMA_USE_EWTS
  logical, parameter :: SACSMA_FALLBACK_LOGGING_ENABLED = .true.
  integer, parameter :: SACSMA_FALLBACK_LOG_LEVEL = LOG_LEVEL_INFO
#endif

  public :: write_log
  public :: is_logger_enabled, get_log_level
  public :: itoa, rtoa

#ifdef SACSMA_USE_EWTS
  logical, save :: did_init = .false.
#else
  type, bind(C) :: c_tm
    integer(c_int) :: tm_sec
    integer(c_int) :: tm_min
    integer(c_int) :: tm_hour
    integer(c_int) :: tm_mday
    integer(c_int) :: tm_mon
    integer(c_int) :: tm_year
    integer(c_int) :: tm_wday
    integer(c_int) :: tm_yday
    integer(c_int) :: tm_isdst
  end type c_tm

  interface
    function c_time(tloc) bind(C, name="time")
      import :: c_long, c_ptr
      integer(c_long) :: c_time
      type(c_ptr), value :: tloc
    end function c_time

    function c_gmtime(timer) bind(C, name="gmtime")
      import :: c_ptr, c_long
      type(c_ptr) :: c_gmtime
      integer(c_long), intent(in) :: timer
    end function c_gmtime
  end interface
#endif

contains

#ifdef SACSMA_USE_EWTS
  subroutine ensure_init()
    if (.not. did_init) then
      call ewts_logger_init_module(EWTS_ID_SAC_SMA)
      did_init = .true.
    end if
  end subroutine ensure_init
#else
  subroutine get_utc_timestamp(ts)
    character(len=*), intent(out) :: ts

    integer :: values(8)
    integer(c_long) :: now
    type(c_ptr) :: tm_ptr
    type(c_tm), pointer :: tm

    call date_and_time(values=values)

    now = c_time(c_null_ptr)
    tm_ptr = c_gmtime(now)

    if (.not. c_associated(tm_ptr)) then
      write(ts, '(A)') "1970-01-01T00:00:00.000Z"
      return
    end if

    call c_f_pointer(tm_ptr, tm)

    write(ts,'(I4.4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2,".",I3.3,"Z")') &
          tm%tm_year + 1900, &
          tm%tm_mon + 1, &
          tm%tm_mday, &
          tm%tm_hour, &
          tm%tm_min, &
          tm%tm_sec, &
          values(8)
  end subroutine get_utc_timestamp

  function level_to_string(level) result(str)
    integer, intent(in) :: level
    character(len=10) :: str

    select case(level)
      case (LOG_LEVEL_DEBUG)
        str = "DEBUG"
      case (LOG_LEVEL_PERFORM)
        str = "PERFORM"
      case (LOG_LEVEL_INFO)
        str = "INFO"
      case (LOG_LEVEL_WARNING)
        str = "WARNING"
      case (LOG_LEVEL_SEVERE)
        str = "SEVERE"
      case (LOG_LEVEL_FATAL)
        str = "FATAL"
      case default
        str = "INFO"
    end select
  end function level_to_string
#endif

  subroutine write_log(message, level)
    character(len=*), intent(in) :: message
    integer, intent(in) :: level

    integer :: mapped_level
#ifndef SACSMA_USE_EWTS
    character(len=32) :: timestamp
#endif

    mapped_level = map_level(level)

#ifdef SACSMA_USE_EWTS
    call ensure_init()
    call ewts_write_log_module(EWTS_ID_SAC_SMA, trim(message), mapped_level)
#else
    if (.not. is_logger_enabled()) return
    if (mapped_level < SACSMA_FALLBACK_LOG_LEVEL) return

    call get_utc_timestamp(timestamp)
    write(*, '(A,1X,A,1X,A,1X,A)') &
      trim(timestamp), "SACSMA", trim(level_to_string(mapped_level)), trim(message)
    flush(6)
#endif
  end subroutine write_log

  logical function is_logger_enabled()
#ifdef SACSMA_USE_EWTS
    call ensure_init()
    is_logger_enabled = ewts_is_logger_enabled_module(EWTS_ID_SAC_SMA)
#else
    is_logger_enabled = SACSMA_FALLBACK_LOGGING_ENABLED
#endif
  end function is_logger_enabled

  integer function get_log_level()
#ifdef SACSMA_USE_EWTS
    call ensure_init()
    get_log_level = ewts_get_log_level_module(EWTS_ID_SAC_SMA)
#else
    get_log_level = SACSMA_FALLBACK_LOG_LEVEL
#endif
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

 module sac_log_module
  use stdlib_logger, global => global_logger

  implicit none      
  !type :: sacLogger
  !public :: sacLogger
  character(128) :: errmsg, message
  character(256) :: iomsg
  integer :: iostat, unit, stat

  !type(logger_type) :: logger


  contains


  function itoa(i) result(res)
    ! This function convrets an integer to ascii format 

    implicit none      
    !class (sacLogger), intent(in) :: this
    character(:),allocatable :: res
    integer,intent(in) :: i
    character(range(i)+2) :: tmp
    write(tmp,'(i0)') i
    res = trim(tmp)
  end function

  
  function rtoa(i) result(res)
    ! This function convrets a real value to ascii format 

    character(:),allocatable :: res
    real,intent(in) :: i
    character*10 format

    data format /'(F10.2)'/
    write(res,format)i
    res = trim(res)
  end function

  subroutine get_utc_time(time_stamp)
    ! This function returns the present time (now) in utc format
    
    use datetime_module, only:datetime
    implicit none

    character(23), intent(out) :: time_stamp
    character(len=5)  :: zone
    integer ::  hr_zone_val
    integer ::  min_zone_val
    integer :: dt(8)
    integer :: year_val, month_val, day_val, hr_val, min_val, sec_val, ms_val
    character(8)  :: date
    character(10) :: time
    real :: zone_val
    type(datetime) :: a

    call date_and_time(values=dt, date=date, time=time, zone=zone)

    read (zone(2:3), '(I10)') hr_zone_val
    read (zone(4:5), '(I10)') min_zone_val
    zone_val = hr_zone_val + (min_zone_val/60)
    if (zone(1:1) == "-") then
      zone_val = -1.0 * zone_val
    end if

    read(date(1:4), '(I10)') year_val
    read(date(5:6), '(I10)') month_val
    read(date(7:8), '(I10)') day_val
    read (time(1:2), '(I10)') hr_val
    read (time(3:4), '(I10)') min_val
    read (time(5:6), '(I10)') sec_val
    read (time(8:10), '(I10)') ms_val

    a = datetime(year_val, month_val, day_val, hr_val, min_val, sec_val, ms_val, tz=zone_val)
    a = a % utc()
    time_stamp = a % isoformat()
  end subroutine get_utc_time


  subroutine get_utc_time_manual(time_stamp)
    !! Returns current date and time in UTC format. Same as get_utc_time, but does not use datetime lib
    
    implicit none
    character(len=5)  :: zone
    integer :: hr_time_val, hr_zone_val
    integer :: min_time_val, min_zone_val
    integer :: dt(8)
    integer :: year_val, month_val, day_val, hr_val, min_val, feb_max_day, max_day_month
    character(23), intent(out) :: time_stamp
    character(8)  :: date
    character(10) :: time
    integer, allocatable :: days_of_months(:)

    call date_and_time(values=dt, date=date, time=time, zone=zone)
    ! print*, "values : ", dt

    read (zone(2:3), '(I10)') hr_zone_val
    read (zone(4:5), '(I10)') min_zone_val
    
    read(date(1:4), '(I10)') year_val
    read(date(5:6), '(I10)') month_val
    read(date(7:8), '(I10)') day_val
    read (time(1:2), '(I10)') hr_time_val
    read (time(3:4), '(I10)') min_time_val

    feb_max_day = 28+(1-min(1,mod(year_val,4)))
    allocate(days_of_months(12))
    days_of_months = (/31,feb_max_day,31,30,31,30,31,31,30,31,30,31/)

    if (zone(1:1) == "-") then
        min_val = min_time_val + min_zone_val
        hr_val = hr_time_val + hr_zone_val
    else
        min_val = min_time_val - min_zone_val
        hr_val = hr_time_val - hr_zone_val
    end if

    if (min_val >= 60) then
      min_val = min_val - 60
      hr_val = hr_val +1
    else if (min_val < 0) then
      min_val = min_val + 60
      hr_val = hr_val -1  
    end if

    if (hr_val >= 24) then
      hr_val = hr_val -24
      day_val = day_val + 1
    else if (hr_val < 0) then
      hr_val = hr_val + 24
      day_val = day_val - 1
    end if

    max_day_month = days_of_months(month_val)
    if (day_val > max_day_month) then
      day_val = day_val - max_day_month
      month_val = month_val + 1
    else if (day_val < 0) then
      day_val = day_val + max_day_month
      month_val = month_val - 1
    end if

    if (month_val > 12) then
      month_val = month_val - 12
      year_val = year_val + 1
    else if (month_val < 0) then
      month_val = month_val + 12
      year_val = year_val - 1
    end if

    write(time_stamp(1:4), '(I4.4)') year_val
    time_stamp(5:5)   = '-'
    write(time_stamp(6:7), '(I2.2)') month_val
    time_stamp(8:8)   = '-'
    write(time_stamp(9:10), '(I2.2)') day_val
    time_stamp(11:11) = 'T'
    write(time_stamp(12:13), '(I2.2)') hr_val
    time_stamp(14:14) = ':'
    time_stamp(15:16) = time(3:4)
    write(time_stamp(15:16), '(I2.2)') min_val
    time_stamp(17:17) = ':'
    time_stamp(18:23) = time(5:10)

    ! print*, time_stamp

  end subroutine get_utc_time_manual


  subroutine get_log_level(log_level, level) 
    ! This function returns log level needed by fortran stdlib 

    implicit none
    integer, intent(out) :: level
    character(8), intent(in) :: log_level

    if (log_level == "INFO") then
      level = 20
    else if (log_level == "WARNING") then
      level = 30
    else if (log_level == "DEBUG") then
      level = 10
    else if (log_level == "ERROR") then
      level = 40
    else
      level = 20
    end if
  end subroutine get_log_level


  subroutine create_logger(log_level)
    ! This is the main function that creates and initialise the log file

    implicit none
    character(100) :: log_file_path
    character(128) :: log_file_name
    character(255) :: cwd
    character(8), intent(in), optional :: log_level
    character(8) :: this_log_level
    character(23) :: time_stamp
    integer :: status
    integer :: level

    ! set the default log level
    if (present(log_level)) then
      this_log_level = log_level
    else
      this_log_level = "INFO"
    end if
    call get_log_level(this_log_level, level)

    ! Determin log file location
    call get_environment_variable("NGEN_LOG_FILE_PATH", log_file_path)
    write (*,*) trim(log_file_path)

    call get_utc_time(time_stamp)
    print*, time_stamp
    if (trim(adjustl(log_file_path)) == "") then
      print*, "log_file_path not found in env. Using local log file path"
      call getcwd(cwd, status)
      if (status == 0) then
        print *, 'Current working directory: ', trim(cwd)
        log_file_path = trim(cwd)//'/' //'run_log'
      else
        print *, 'Error retrieving current directory'
        stop
      end if
    end if

    log_file_path = trim(log_file_path)//'/' //time_stamp(1:10)

    ! create the directories needed if not already exists
    CALL system("mkdir -p " // log_file_path)

    log_file_name = trim(log_file_path)// '/' // 'sac_log_' // time_stamp(12:13)//time_stamp(15:16)//time_stamp(18:19)//'.txt'

    print *, 'log file name: ' // log_file_name
    call global%add_log_file(log_file_name, unit, &
                           position='asis', stat=stat)
    if (stat /= success) then
      error stop 'Unable to open ' // log_file_name // '.'
    end if

    print*, "logger created"  
    open(newunit=unit, file=log_file_name, form='formatted', status='replace', &
          position='rewind', action='write', iostat=iostat, iomsg=iomsg)
    print*, stat


    call global%add_log_unit(unit, stat)

    select case ( stat )
      case ( read_only_error )
         error stop 'Unable to write to ' // log_file_name // '.'
      end select

    call global%configure(time_stamp=.false.)
  end subroutine create_logger


  subroutine write_log(msg, msg_type)
   !this subroutine writes the log message to the log file

   implicit none 
   character(len=*), intent(in)        :: msg
   character(len=*), intent(in)        :: msg_type
   character(len=23) :: utc_time
   character(len=128) :: log_msg

   ! get the present time and create the message that will be sent to the log file
   call get_utc_time(utc_time)
   log_msg = utc_time // " SACSMA " //  msg_type // " " //    msg

   call global % log_message( log_msg)
  end subroutine write_log

 end module sac_log_module

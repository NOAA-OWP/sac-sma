 module sac_log_module

  implicit none      
  character(128) :: errmsg, message
  character(256) :: iomsg
  integer :: iostat, unit, stat
  character(:), allocatable :: log_file_name
  ! Set Default Log Level: INFO=20, DEBUG=10, WARN=30, ERROR=40, FATAL=50
  integer :: default_log_level=20

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
    character(128) format

    data format /'(F10.10)'/
    write(res,format)i
    res = trim(res)
  end function

  function r8toa(i) result(res)
    ! This function convrets a real value to ascii format

    character(128) :: res
    real(KIND=8),intent(in) :: i
    character(128) format

    data format /'(F10.10)'/
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
    double precision :: zone_val
    type(datetime) :: a

    call date_and_time(values=dt, date=date, time=time, zone=zone)

    read (zone(2:3), '(I10)') hr_zone_val
    read (zone(4:5), '(I10)') min_zone_val
    zone_val = hr_zone_val + (min_zone_val/60)
    if (zone(1:1) == "-") then
      zone_val = -1.0 * zone_val
    end if

    read(date(1:4), '(I4)') year_val
    read(date(5:6), '(I10)') month_val
    read(date(7:8), '(I10)') day_val
    read (time(1:2), '(I10)') hr_val
    read (time(3:4), '(I10)') min_val
    read (time(5:6), '(I10)') sec_val
    read (time(8:10), '(I10)') ms_val

    a = datetime(year=year_val, month=month_val, day=day_val, hour=hr_val, minute=min_val, second=sec_val, millisecond=ms_val, tz=zone_val)
    a = a % utc()
    time_stamp = trim(a % isoformat())
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


  subroutine get_log_level(log_level_str, log_level) 
    ! This function returns log level needed by fortran stdlib 

    implicit none
    integer, intent(out) :: log_level
    ! character(20), intent(in) :: log_level_str
    character(len=*), intent(in) :: log_level_str

    if (trim(log_level_str) == "INFO") then
      log_level = 20
    else if (trim(log_level_str) == "WARN") then
      log_level = 30
    else if (trim(log_level_str) == "DEBUG") then
      log_level = 10
    else if (trim(log_level_str) == "ERROR") then
      log_level = 40
    else if (trim(log_level_str) == "FATAL") then
      log_level = 50
    else
      log_level = 20
    end if
  end subroutine get_log_level


  subroutine get_log_file_name()
    implicit none
    character(128) :: temp_path
    character(255) :: log_file_path
    character(255) :: cwd
    character(23) :: time_stamp
    integer :: status
    logical :: exist

    ! Determin log file location
    call get_environment_variable("NGEN_LOG_FILE_PATH", log_file_path)
    write (*,*) trim(log_file_path)

    call get_utc_time(time_stamp)

    if (trim(adjustl(log_file_path)) == "") then
      print*, "log_file_path not found in env. Using local log file path"
      call getcwd(cwd, status)
      if (status == 0) then
        print *, 'Current working directory: ', trim(cwd)
        temp_path = trim(cwd)//'/' //'run_log/' //time_stamp(1:10)
        CALL system("mkdir -p " // temp_path)
        log_file_path = trim(temp_path)// '/' // 'sac_log_' // time_stamp(12:13)//time_stamp(15:16)//time_stamp(18:19)//'.txt'
        print*, "log_file_path : "//log_file_path
      else
        print *, 'Error retrieving current directory'
        stop
      end if
    end if


    if (len_trim(log_file_name) == 0) then
      allocate(character(len(trim(log_file_path))) :: log_file_name)
    end if
    log_file_name = trim(log_file_path)
  end subroutine get_log_file_name


  subroutine create_logger(log_level_str)
    ! This is the main function that creates and initialise the log file

    implicit none
    character(len=*), intent(in), optional :: log_level_str
    integer :: status
    logical :: exist
    integer :: log_level
    character(8) :: this_log_level_str

    ! set the default log level
    call get_environment_variable("sac-sma_ll", this_log_level_str)
    if (trim(adjustl(this_log_level_str)) == "") then
      print*, "Log level not found in environment variable, will use default log level"
    else
      print*, "Log level found in environment : " //this_log_level_str
      call get_log_level(trim(adjustl(this_log_level_str)), log_level)
      default_log_level = log_level
    end if

    print*, "Default log level :", default_log_level


    if(len_trim(log_file_name) == 0) then
      call get_log_file_name()
    end if

    print *, 'log file name: ' // log_file_name
    !inquire(file=log_file_name, exist=exist)
    !if (exist) then
    !        print *, 'log file already exists'
    !end if
  end subroutine create_logger


  subroutine write_log(msg, log_level_str)
   !this subroutine writes the log message to the log file

   implicit none 
   character(len=*), intent(in)        :: msg
   character(len=*), intent(in)        :: log_level_str
   character(len=23) :: utc_time
   character(len=128) :: log_msg
   integer :: log_level

   logical :: exist

   call get_log_level(log_level_str, log_level)
   if (log_level < default_log_level) return

   ! get the present time and create the message that will be sent to the log file
   call get_utc_time(utc_time)
   log_msg = trim(utc_time) //achar(9) // "SACSMA"//achar(9)//  log_level_str // achar(9)// msg
   if(len_trim(log_file_name) == 0) then
     call get_log_file_name()
     print *, 'log file name: ' // log_file_name
   end if

   
   inquire(file=log_file_name, exist=exist)
   if (exist) then
    open(12, file=log_file_name, status="old", position="append", action="write")
   else
    open(12, file=log_file_name, status="new", action="write")
  end if

  write(12, '(A)')trim(log_msg)
  close(12)

  end subroutine write_log

 end module sac_log_module

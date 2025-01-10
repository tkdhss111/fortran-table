!========================================================
! Date and Time Fortran Module
! 
! @brief 
!  Almost pure elemental procedure
!
! @author Hisashi Takeda, Ph.D.
!
! @date 2020-04-11
module dt_mo

  implicit none

  private
  public :: dt_ty, diff_dt_ty, diff_dt, seq_dt, get_datetime
  public :: diff_days, diff_hrs, diff_mins
  public :: strptime, strftime, print_dts
  public :: get_rec_date, get_pos_rec
  public :: LAB_WDAY, LAB_WDAY_JP, leap_yr
  public :: count_days
  public :: day_of_the_week
  public :: tm_ty

  integer,      parameter :: EPOCHYEAR      = 2000
  character(3), parameter :: LAB_WDAY(7)    = ["Sun", "Mon", "Tue", "Wed", "Thr", "Fri", "Sat"]
  character(3), parameter :: LAB_WDAY_JP(7) = ["日", "月", "火", "水", "木", "金", "土"]
  integer,      parameter :: NSECS_D        = 60 * 60 * 24
  integer,      parameter :: NSECS_H        = 60 * 60
  integer,      parameter :: NSECS_M        = 60
  integer,      parameter :: NSECS_S        = 1

  type dt_ty
    ! Size: 19 + 10 + 8 + 4 + 2 * 6 + 4 * 10 = 93 bytes
    character(80) :: nm = ''
    character(19) :: datetime
    character(10) :: date
    character(8)  :: time
    character(4)  :: yyyy
    character(2)  :: yy, mm, dd, hh, nn, ss
    integer       :: yr, mo, dy, hr, mi, sc, dsc
    integer       :: dow, doy, leap
  contains
    procedure, nopass :: now   => set_now
    procedure         :: minus => minus_dt
    procedure         :: plus  => plus_dt
    procedure         :: diff_secs
    generic           :: operator(-) => diff_secs
  end type

  type diff_dt_ty
    ! N.B. There is no yrs because the number of days in each yr is different.
    integer :: mons, days
    integer :: hrs, mins, secs
  end type

  ! Timer
  type tm_ty
    integer :: start = 0, end = 0, rate = 0, max = 0, diff = 0
    character(255) :: msg
  contains
    procedure tic
    procedure toc
  end type

contains

  pure elemental integer function get_rec_date ( date )
    character(10), intent(in) :: date
    type(dt_ty) t
    t = strptime( date, fmt = '%Y-%m-%d' )
    get_rec_date = count_days ( yr = t%yr, mo = t%mo, dy = t%dy )
  end function

  pure elemental integer function get_pos_rec ( x, secs0 )
    !
    ! Get direct access position of a record of a certain time from the epoch
    !
    class(*),          intent(in) :: x
    integer, optional, intent(in) :: secs0 ! Time delta in seconds
    type(dt_ty)                   :: dt
    integer                       :: ndays, nsecs, secs
    select type ( x )
      type is ( character(*) )
        dt = strptime ( x )
      type is ( dt_ty )
        dt = x 
    end select
    if ( present( secs0 ) ) then
      secs = secs0
    else
      secs = 30 * NSECS_M 
    end if
    ndays = count_days ( yr = dt%yr, mo = dt%mo, dy = dt%dy ) - 1 ! til previous day
    nsecs = ndays * NSECS_D + dt%hr * NSECS_H + dt%mi * NSECS_M + dt%sc
    get_pos_rec = 1 + int( nsecs / secs )
  end function

  pure elemental function diff_dt ( dt_fr, dt_to ) result ( n )
    type(dt_ty), intent(in) :: dt_fr
    type(dt_ty), intent(in) :: dt_to
    type(diff_dt_ty)        :: n
    n%secs = diff_secs ( dt_to, dt_fr )
    n%days = int( n%secs / NSECS_D )
    n%secs = n%secs - n%days * NSECS_D
    n%hrs  = int( n%secs / NSECS_H )
    n%secs = n%secs - n%hrs * NSECS_H
    n%mins  = int( n%secs / NSECS_M )
    n%secs = n%secs - n%mins * NSECS_M
  end function

  pure elemental function diff_days ( dt_fr, dt_to ) result ( ndays )
    type(dt_ty), intent(in) :: dt_fr
    type(dt_ty), intent(in) :: dt_to
    integer ndays
    ndays = int( diff_secs ( dt_to, dt_fr ) / NSECS_D )
  end function

  pure elemental function diff_hrs ( dt_fr, dt_to ) result ( nhrs )
    type(dt_ty), intent(in) :: dt_fr
    type(dt_ty), intent(in) :: dt_to
    integer nhrs
    nhrs = int( diff_secs ( dt_to, dt_fr ) / NSECS_H )
  end function

  pure elemental function diff_mins ( dt_fr, dt_to ) result ( nmins )
    type(dt_ty), intent(in) :: dt_fr
    type(dt_ty), intent(in) :: dt_to
    integer nmins
    nmins = int( diff_secs ( dt_to, dt_fr ) / NSECS_M )
  end function

  pure elemental integer function diff_secs ( dt_to, dt_fr )
    class(dt_ty), intent(in) :: dt_to
    type(dt_ty),  intent(in) :: dt_fr
    integer                  :: ndays_fr, ndays_to
    ndays_fr = count_days ( yr = dt_fr%yr, mo = dt_fr%mo, dy = dt_fr%dy, epochyr = dt_fr%yr ) - 1 ! til previous day
    ndays_to = count_days ( yr = dt_to%yr, mo = dt_to%mo, dy = dt_to%dy, epochyr = dt_fr%yr ) - 1 ! Epoch year is same
    ! N.B. By doing the following, we can avoid overflow of integer(4)
    diff_secs = (ndays_to - ndays_fr) * NSECS_D &
              + (dt_to%hr - dt_fr%hr) * NSECS_H &
              + (dt_to%mi - dt_fr%mi) * NSECS_M &
              + (dt_to%sc - dt_fr%sc)
  end function

  pure function seq_dt ( dt_fr, dt_to, by ) result ( dts )
    type(dt_ty),  intent(in) :: dt_fr, dt_to
    character(*), intent(in) :: by
    type(dt_ty), allocatable :: dts(:)
    character(10)            :: cunit
    character(19), allocatable :: cts(:)
    integer i, p, step, nsteps, n 
    n = dt_to - dt_fr
    p = index ( by, ' ' )
    read ( by(1:p-1), * ) step
    cunit = trim(by(p + 1:))
    select case ( cunit )
      case ( 'd', 'day', 'days' )
        nsteps = 1 + int( n / NSECS_D / step )
        allocate ( dts(nsteps) )
        dts(1) = dt_fr
        do i = 1, nsteps - 1 
          dts(i + 1) = plus_days ( dts(i), step )
        end do
      case ( 'h', 'hr', 'hrs', 'hour', 'hours'  )
        nsteps = 1 + int( n / NSECS_H / step )
        allocate ( dts(nsteps) )
        dts(1) = dt_fr
        do i = 1, nsteps - 1 
          dts(i + 1) = plus_hrs ( dts(i), step )
        end do
      case ( 'm', 'mi', 'min', 'mins', 'minute', 'minutes' )
        nsteps = 1 + int( n / NSECS_M / step )
        allocate ( dts(nsteps) )
        dts(1) = dt_fr
        do i = 1, nsteps - 1 
          dts(i + 1) = plus_mins ( dts(i), step )
        end do
      case default 
        ! N.B. This can overflow integer(4) for 68 yrs
        ! -2,147,483,648 ～ 2,147,483,647 
        nsteps = 1 + int( n / step )
        allocate ( dts(nsteps) )
        dts(1) = dt_fr
        do i = 1, nsteps - 1 
          dts(i + 1) = plus_secs ( dts(i), step )
        end do
    end select
    cts = get_datetime ( yr = dts%yr, mo = dts%mo, dy = dts%dy, hr = dts%hr, mi = dts%mi, sc = dts%sc )
    dts = strptime ( cts )
    ! N.B. The following implementation is deprecated (sometimes, tihs causes segmentation fault).
    !dts = strptime ( get_datetime ( yr = dts%yr, mo = dts%mo, dy = dts%dy, hr = dts%hr, mi = dts%mi, sc = dts%sc ) )
  end function

  pure elemental character(19) function get_datetime ( yr, mo, dy, hr, mi, sc )
    integer, optional, intent(in) :: yr, mo, dy, hr, mi, sc
    integer                       :: yr_, mo_, dy_, hr_, mi_, sc_
    character(4)                  :: yyyy
    character(2)                  :: mm, dd, hh, nn, ss
    yr_ = EPOCHYEAR
    mo_ = 1
    dy_ = 1
    hr_ = 0
    mi_ = 0
    sc_ = 0
    if ( present(yr) ) yr_ = yr
    if ( present(mo) ) mo_ = mo
    if ( present(dy) ) dy_ = dy
    if ( present(hr) ) hr_ = hr
    if ( present(mi) ) mi_ = mi
    if ( present(sc) ) sc_ = sc
    write ( yyyy, "(i4)" ) yr_
    write ( mm, "(i2.2)" ) mo_
    write ( dd, "(i2.2)" ) dy_
    write ( hh, "(i2.2)" ) hr_
    write ( nn, "(i2.2)" ) mi_
    write ( ss, "(i2.2)" ) sc_
    get_datetime = yyyy//"-"//mm//"-"//dd//" "//hh//":"//nn//":"//ss
  end function

  pure elemental function strptime ( str, fmt ) result ( dt )
    character(*),           intent(in) :: str
    character(*), optional, intent(in) :: fmt
    character(19)                      :: fmt_
    type(dt_ty)                        :: dt
    integer i_f, i_s
    if ( present(fmt) ) then
      fmt_ = trim(fmt)
    else
      fmt_ = '%Y-%m-%d %H:%M:%S'
    end if
    write( dt%yyyy, '(i4)' ) EPOCHYEAR
    dt%yy = dt%yyyy(3:4)
    dt%mm = '01'
    dt%dd = '01'
    dt%hh = '00'
    dt%nn = '00'
    dt%ss = '00'
    i_f = 1
    i_s = 0
    do while ( i_f <= len_trim( fmt_ ) )
      if ( fmt_(i_f:i_f) /= '%' ) then
        i_s = i_s + 1 
        i_f = i_f + 1
      else
        select case ( fmt_(i_f + 1:i_f + 1) )
          case ( 'Y' )
            dt%yyyy = str(i_s + 1:i_s + 4)
            dt%yy   = str(i_s + 3:i_s + 4)
            i_s = i_s + 4
          case ( 'y' )
            dt%yyyy(3:4) = str(i_s + 1:i_s + 2)
            dt%yy        = str(i_s + 1:i_s + 2)
            i_s = i_s + 2
          case ( 'm' )
             dt%mm = str(i_s + 1:i_s + 2)
            i_s = i_s + 2
          case ( 'd' )
            dt%dd = str(i_s + 1:i_s + 2)
            i_s = i_s + 2
          case ( 'H' )
            dt%hh = str(i_s + 1:i_s + 2)
            i_s = i_s + 2
          case ( 'M' )
            dt%nn = str(i_s + 1:i_s + 2)
            i_s = i_s + 2
          case ( 'S' )
            dt%ss = str(i_s + 1:i_s + 2)
            i_s = i_s + 2
        end select
        i_f = i_f + 2
      end if
    end do
    dt%date     = dt%yyyy//"-"//dt%mm//"-"//dt%dd
    dt%time     = dt%hh//":"//dt%nn//":"//dt%ss
    dt%datetime = dt%date//" "//dt%time
    read ( dt%yyyy, * ) dt%yr
    read ( dt%mm,   * ) dt%mo
    read ( dt%dd,   * ) dt%dy
    read ( dt%hh,   * ) dt%hr
    read ( dt%nn,   * ) dt%mi
    read ( dt%ss,   * ) dt%sc
    dt%leap = leap_yr ( dt%yr )
    if ( dt%mo /= 0 .and. dt%dy /= 0 ) then
      dt%dow = day_of_the_week ( dt%yr, dt%mo, dt%dy )
    end if
    if ( dt%mo /= 0 .and. dt%dy /= 0 ) then
      dt%doy = count_days ( dt%yr, dt%mo, dt%dy, epochyr = dt%yr )
    end if
    dt%dsc = dt%hr * 60 * 60 + dt%mi * 60 + dt%sc
  end function

  pure elemental function strftime ( dt, fmt ) result ( str )
    ! N.B. keep this function standalone ( not type-bound procedure )
    !      for arrays process.
    type(dt_ty),  intent(in) :: dt
    character(*), intent(in) :: fmt
    character(len(fmt)*2)    :: str
    character(1)             :: c
    integer i_f, i_s
    i_f = 1
    i_s = 0
    str = ''
    do while ( i_f <= len_trim( fmt ) )
      c = fmt(i_f:i_f)
      if ( c /= '%' ) then
        str(i_s + 1:i_s + 1) = c
        i_s = i_s + 1 
        i_f = i_f + 1
      else
        select case ( fmt(i_f + 1:i_f + 1) )
          case ( 'Y' )
            str(i_s + 1:i_s + 4) = dt%yyyy
            i_s = i_s + 4
          case ( 'y' )
            str(i_s + 1:i_s + 2) = dt%yy
            i_s = i_s + 2
          case ( 'm' )
            str(i_s + 1:i_s + 2) = dt%mm
            i_s = i_s + 2
          case ( 'd' )
            str(i_s + 1:i_s + 2) = dt%dd
            i_s = i_s + 2
          case ( 'H' )
            str(i_s + 1:i_s + 2) = dt%hh
            i_s = i_s + 2
          case ( 'M' )
            str(i_s + 1:i_s + 2) = dt%nn
            i_s = i_s + 2
          case ( 'S' )
            str(i_s + 1:i_s + 2) = dt%ss
            i_s = i_s + 2
          case ( 'a' )
            str(i_s + 1:i_s + 3) = LAB_WDAY_JP(dt%dow)
            i_s = i_s + 3
        end select
        i_f = i_f + 2
      end if
    end do
  end function

  subroutine print_dts ( dts )
    type(dt_ty), intent(in) :: dts(:) 
    integer i, n
    n = size(dts)
    print"(a)", "========= Date and Time =========="
    do i = 1, n
      if ( n > 5 ) then
        if ( i == 6 ) then
          print*, "    :"
          print*, "    :"
        end if
        if ( i <= 5 .or. i > n - 5 ) then
          print"( *(a) )", dts(i)%datetime, " (", LAB_WDAY(dts(i)%dow), ")"
        end if
      else
        print"( *(a) )", dts(i)%datetime, " (", LAB_WDAY(dts(i)%dow), ")"
      end if
    end do
    print"(a)", "=================================="
  end subroutine

  type(dt_ty) function set_now ( )
    character(8)  :: date
    character(10) :: time
    character(19) :: datetime
    call date_and_time ( date = date, time = time )
    write ( datetime, '(a)' ) &
      date(1:4)//'-'//date(5:6)//'-'//date(7:8)//' '//time(1:2)//':'//time(3:4)//':'//time(5:6)
    set_now = strptime ( datetime ) 
  end function
  
  pure elemental function plus_dt ( dt, days, hrs, mins, secs ) result ( dtp )
    class(dt_ty),      intent(in) :: dt
    type(dt_ty)                   :: dtp
    integer, optional, intent(in) :: days, hrs, mins, secs
    dtp = dt
    if ( present( days ) ) dtp = plus_days ( dtp, days )
    if ( present( hrs  ) ) dtp = plus_hrs  ( dtp, hrs  )
    if ( present( mins ) ) dtp = plus_mins ( dtp, mins )
    if ( present( secs ) ) dtp = plus_secs ( dtp, secs )
    dtp%datetime = get_datetime ( dtp%yr, dtp%mo, dtp%dy, dtp%hr, dtp%mi, dtp%sc )
    dtp = strptime ( dtp%datetime )
  end function
    
  pure elemental function minus_dt ( dt, days, hrs, mins, secs ) result ( dtp )
    class(dt_ty),      intent(in) :: dt
    type(dt_ty)                   :: dtp
    integer, optional, intent(in) :: days, hrs, mins, secs
    dtp = dt
    if ( present( days ) ) dtp = minus_days ( dtp, days )
    if ( present( hrs  ) ) dtp = minus_hrs  ( dtp, hrs  )
    if ( present( mins ) ) dtp = minus_mins ( dtp, mins )
    if ( present( secs ) ) dtp = minus_secs ( dtp, secs )
    dtp%datetime = get_datetime ( dtp%yr, dtp%mo, dtp%dy, dtp%hr, dtp%mi, dtp%sc )
    dtp = strptime ( dtp%datetime )
  end function
    
  pure elemental function plus_days ( dt, days ) result ( dtp )
    class(dt_ty), intent(in) :: dt
    type(dt_ty)              :: dtp
    integer,      intent(in) :: days
    integer i
    dtp = dt
    do i = 1, days
      dtp%dy = dtp%dy + 1
      if ( dtp%dy <= ndays_mon ( dtp%yr, dtp%mo ) ) cycle
      dtp%mo = dtp%mo + 1
      if ( dtp%mo > 12 ) then
        dtp%yr = dtp%yr + 1
        dtp%mo = 1
      end if
      dtp%dy = 1
    end do
  end function

  pure elemental function minus_days ( dt, days ) result ( dtp )
    class(dt_ty), intent(in) :: dt
    type(dt_ty)              :: dtp
    integer,      intent(in) :: days
    integer i
    dtp = dt
    do i = 1, days
      dtp%dy = dtp%dy - 1
      if ( dtp%dy > 0 ) cycle
      dtp%mo = dtp%mo - 1
      if ( dtp%mo < 1 ) then
        dtp%yr = dtp%yr - 1
        dtp%mo = 12
      end if
      dtp%dy = ndays_mon ( dtp%yr,  dtp%mo )
    end do
  end function
  
  pure elemental function plus_hrs ( dt, hrs ) result ( dtp )
    class(dt_ty), intent(in) :: dt
    type(dt_ty)              :: dtp
    integer,      intent(in) :: hrs
    integer i
    dtp = dt
    do i = 1, hrs
      dtp%hr = dtp%hr + 1
      if ( dtp%hr < 24 ) cycle
      dtp%dy = dtp%dy + 1
      if ( dtp%dy > ndays_mon ( dtp%yr, dtp%mo )) then
        dtp%mo = dtp%mo + 1
        if ( dtp%mo > 12 ) then
          dtp%yr = dtp%yr + 1
          dtp%mo = 1
        end if
        dtp%dy = 1
      end if
      dtp%hr = 0
    end do
  end function

  pure elemental function minus_hrs ( dt, hrs ) result ( dtp )
    class(dt_ty), intent(in) :: dt
    type(dt_ty)              :: dtp
    integer,      intent(in) :: hrs
    integer i
    dtp = dt
    do i = 1, hrs
      dtp%hr = dtp%hr - 1
      if (dtp%hr >= 0 ) cycle
      dtp%dy = dtp%dy - 1
      if ( dtp%dy < 1 ) then
        dtp%mo  = dtp%mo - 1
        if ( dtp%mo < 1 ) then
          dtp%yr = dtp%yr - 1
          dtp%mo = 12
        end if
        dtp%dy = ndays_mon ( dtp%yr,  dtp%mo )
      end if
      dtp%hr = 23
    end do
  end function

  pure elemental function plus_mins ( dt, mins ) result (dtp)
    class(dt_ty), intent(in) :: dt
    type(dt_ty)              :: dtp
    integer,      intent(in) :: mins
    integer i
    dtp = dt
    do i = 1, mins
      dtp%mi = dtp%mi + 1
      if ( dtp%mi < 60 ) cycle
      dtp%hr = dtp%hr + 1
      if ( dtp%hr > 23 ) then
        dtp%dy = dtp%dy + 1
        if ( dtp%dy > ndays_mon ( dtp%yr, dtp%mo ) ) then
          dtp%mo = dtp%mo + 1
          if ( dtp%mo > 12 ) then
            dtp%yr = dtp%yr + 1
            dtp%mo = 1
          end if
          dtp%dy = 1
        end if
        dtp%hr = 0
      end if
      dtp%mi = 0
    end do
  end function

  pure elemental function minus_mins ( dt, mins ) result ( dtp )
    class(dt_ty), intent(in) :: dt
    type(dt_ty)              :: dtp
    integer,      intent(in) :: mins
    integer i
    dtp = dt
    do i = 1, mins
      dtp%mi = dtp%mi - 1
      if ( dtp%mi >= 0 ) cycle
      dtp%hr = dtp%hr - 1
      if (dtp%hr < 0 ) then
        dtp%dy = dtp%dy - 1
        if ( dtp%dy < 1 ) then
          dtp%mo = dtp%mo - 1
          if ( dtp%mo < 1 ) then
            dtp%yr = dtp%yr - 1
            dtp%mo = 12
          end if
          dtp%dy = ndays_mon ( dtp%yr,  dtp%mo )
        end if
        dtp%hr = 23
      end if
      dtp%mi = 59
    end do
  end function

  pure elemental function plus_secs ( dt, secs ) result ( dtp )
    class(dt_ty), intent(in) :: dt
    type(dt_ty)              :: dtp
    integer,      intent(in) :: secs
    integer i
    dtp = dt
    do i = 1, secs
      dtp%sc = dtp%sc + 1
      if ( dtp%sc < 60 ) cycle
      dtp%mi = dtp%mi + 1
      if ( dtp%mi > 59 ) then
        dtp%hr = dtp%hr + 1
        if ( dtp%hr > 23 ) then
          dtp%dy = dtp%dy + 1
          if ( dtp%dy > ndays_mon ( dtp%yr, dtp%mo ) ) then
            dtp%mo = dtp%mo + 1
            if ( dtp%mo > 12 ) then
              dtp%yr = dtp%yr + 1
              dtp%mo = 1
            end if
            dtp%dy = 1
          end if
          dtp%hr = 0
        end if
        dtp%mi = 0
      end if
      dtp%sc = 0
    end do
  end function

  pure elemental function minus_secs ( dt, secs ) result ( dtp )
    class(dt_ty), intent(in) :: dt
    type(dt_ty)              :: dtp
    integer,      intent(in) :: secs
    integer i
    dtp = dt
    do i = 1, secs
      dtp%sc = dtp%sc - 1
      if ( dtp%sc >= 0 ) cycle
      dtp%mi = dtp%mi - 1
      if ( dtp%mi < 0 ) then
        dtp%hr = dtp%hr - 1
        if (dtp%hr < 0 ) then
          dtp%dy = dtp%dy - 1
          if ( dtp%dy < 1 ) then
            dtp%mo  = dtp%mo - 1
            if ( dtp%mo < 1 ) then
              dtp%yr = dtp%yr - 1
              dtp%mo = 12
            end if
            dtp%dy = ndays_mon ( dtp%yr, dtp%mo )
          end if
          dtp%hr = 23
        end if
        dtp%mi = 59
      end if
      dtp%sc = 59
    end do
  end function

  pure elemental logical function is_leap_yr ( yr )
    integer, intent(in) :: yr
    !is_leap_yr = ( mod(yr, 4) == 0 .and. .not. mod(yr, 100) == 0 ) .or. mod(yr, 400) == 0 ! Precise but very slow
    !is_leap_yr = findloc( LEAPYEARS, yr, dim = 1 ) > 0
    is_leap_yr = mod( yr, 4 ) == 0 ! Best so far
  end function

  pure elemental integer function leap_yr ( yr )
    integer, intent(in) :: yr
    !if ( ( mod(yr, 4) == 0 .and. .not. mod(yr, 100) == 0 ) .or. mod(yr, 400) == 0 ) then ! Precise but very slow
    if ( mod( yr, 4 ) == 0 ) then
      leap_yr = 1
    else
      leap_yr = 0
    end if
  end function

  pure elemental integer function day_of_the_week ( yr, mo, dy )
    integer, intent(in) :: yr, mo, dy
    integer j, k, yy, mm
    !
    ! Zeller''s congruence
    !
    ! https://www.rosettacode.org/wiki/Day_of_the_week#Fortran
    !
    yy = yr
    mm = mo
    if ( mm <= 2 ) then
      yy = yy - 1
      mm = mm + 12
    end if
    j = yy / 100
    k = mod( yy, 100 )
    day_of_the_week  = mod(dy + ((mm+1)*26)/10 + k + k/4 + j/4 + 5*j, 7)
    if ( day_of_the_week == 0 ) then
      day_of_the_week = 7
    end if
  end function

  pure elemental integer function ndays_mon ( yr, mo )
    integer, intent(in) :: yr, mo
    integer, parameter  :: ndays_mons(12) = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    if ( mo == 2 ) then
      if ( is_leap_yr ( yr ) ) then
        ndays_mon = ndays_mons ( mo ) + 1
        return
      end if
    end if
    ndays_mon = ndays_mons ( mo )
  end function

  pure elemental integer function count_days ( yr, mo, dy, epochyr )

    integer, intent(in)           :: yr, mo, dy
    integer, intent(in), optional :: epochyr
    integer                       :: n, i_y, i_m, epochyr_

    if ( present( epochyr ) ) then
      epochyr_ = epochyr
    else
      epochyr_ = EPOCHYEAR
    end if

    ! DO NOT REMOVE (e.g., used for indexing by count(n%days < 0) by other applications )
    if ( yr < epochyr_ ) then
      count_days = -999
      return
    end if

    n = 0

    ! Adding up number of days at the end of the year after epoch year 
    if ( yr > epochyr_ ) then
      do i_y = epochyr_, yr - 1
        n = n + 365
        if ( is_leap_yr ( i_y ) ) n = n + 1
      end do
    end if

    ! Number of days at the end of the last month
    ! If it is epoch year, then only this is processed.
    if ( mo > 1 ) then
      do i_m = 1, mo - 1
        n = n + ndays_mon ( yr, i_m )
      end do
    end if

    ! Present month
    count_days = n + dy

  end function

  !===================================================
  ! Timer
  !
  subroutine tic ( this )
    class (tm_ty) this
    call system_clock ( this%start ) ! Get system time
    this%msg = 'NA'
  end subroutine

  subroutine toc ( this )
    class (tm_ty) this
    call system_clock ( this%end, this%rate, this%max )
    if ( this%end < this%start ) then
      this%diff = ( this%max - this%start ) + this%end + 1
    else
      this%diff = this%end - this%start
    end if
    write ( this%msg, '(a, f10.2, a)' ) "Elapsed time: ", this%diff / real(this%rate), " seconds"
    print*, trim(this%msg)
  end subroutine

end module

module table_mo

  implicit none

  private
  public :: LEN_C
  public :: table_ty
  public :: select, filter, delete
  public :: insert_col
  public :: inner_join, left_join, right_join
  public :: inner_join_pure, left_join_pure, right_join_pure
  public :: insert_or_replace, append
  public :: union, intersect
  public :: sort_integer, sort_character
  public :: get_cells_from_csvline, get_csvline_from_cells
  public :: count_rows, count_cols, count_seps
  public :: read_csv, csv2parquet
  public :: is_eq, is_numeric, round

  integer,          parameter :: LEN_C = 23     ! Character length of each cell (N.B. care memory size)
  character(LEN_C), parameter :: cNA   = 'NA'   ! N/A data notation for character
  integer,          parameter :: iNA   = -999   ! N/A data notation for integer
  real,             parameter :: rNA   = -999.0 ! N/A data notation for integer

  type table_ty

    integer                       :: nrows = 0
    integer                       :: ncols = 0
    character(LEN_C), allocatable :: rownames(:), colnames(:)
    character(LEN_C), allocatable :: cell(:, :)
    character(LEN_C)              :: key  = ''
    character(255)                :: name, file

  contains
    procedure :: index_row, index_col
    procedure :: init  => init_table
    procedure :: select_integer, select_logical, select_character
    generic   :: select => select_integer, select_logical, select_character
    procedure :: filter_integer, filter_logical, filter_character
    generic   :: filter => filter_integer, filter_logical, filter_character
    procedure :: delete_integer, delete_logical, delete_character
    generic   :: delete => delete_integer, delete_logical, delete_character
    procedure :: insert_col
    procedure :: to_character_colindex, to_logical_colindex, to_integer_colindex, to_real_colindex
    procedure :: to_character_colname,  to_logical_colname,  to_integer_colname,  to_real_colname
    generic   :: to_character   => to_character_colindex,    to_character_colname 
    generic   :: to_logical     => to_logical_colindex,      to_logical_colname 
    generic   :: to_integer     => to_integer_colindex,      to_integer_colname 
    generic   :: to_real        => to_real_colindex,         to_real_colname 
    procedure :: from_character_colindex, from_logical_colindex, from_integer_colindex, from_real_colindex
    procedure :: from_character_colname,  from_logical_colname,  from_integer_colname,  from_real_colname
    generic   :: from_character => from_character_colindex, from_character_colname 
    generic   :: from_logical   => from_logical_colindex,   from_logical_colname 
    generic   :: from_integer   => from_integer_colindex,   from_integer_colname 
    generic   :: from_real      => from_real_colindex,      from_real_colname 
    procedure :: inner_join, left_join, right_join
    procedure :: insert_or_replace, append
    procedure :: write_csv, read_csv, write_parquet
    procedure :: print => print_table
    generic, public :: operator(+) => insert_or_replace
    generic, public :: operator(*) => left_join

  end type

  ! Public functions
  interface select
    procedure :: select_integer, select_logical, select_character
  end interface

  interface filter
    procedure :: filter_integer, filter_logical, filter_character
  end interface

  interface delete
    procedure :: delete_integer, delete_logical, delete_character
  end interface

  interface sort_integer
    procedure :: sort_integer4, sort_integer8 
  end interface

contains

  pure subroutine init_table ( this, nrows, ncols, cell, rownames, colnames, name, key, file )

    class(table_ty),           intent(inout) :: this
    integer,                   intent(in)    :: nrows, ncols
    character(*),    optional, intent(in)    :: cell(:, :)
    character(*),    optional, intent(in)    :: rownames(:), colnames(:)
    character(*),    optional, intent(in)    :: name, key, file

    if ( allocated( this%rownames ) ) deallocate( this%rownames )
    if ( allocated( this%colnames ) ) deallocate( this%colnames )
    if ( allocated( this%cell     ) ) deallocate( this%cell     )

    allocate( this%rownames(nrows) )
    allocate( this%colnames(ncols) )
    allocate( this%cell(nrows, ncols) )

    this%nrows = nrows
    this%ncols = ncols

    if ( present( cell ) ) then
      this%cell = cell
    else
      this%cell = ''
    end if

    if ( present( rownames ) ) then
      this%rownames = rownames
    else
      this%rownames = ''
    end if

    if ( present( colnames ) ) then
      this%colnames = colnames
    else
      this%colnames = ''
    end if

    if ( present( name ) ) then
      this%name = name
    else
      this%name = ''
    end if

    if ( present( key ) ) then
      this%key = key
    else
      this%key = this%colnames(1)
    end if

    if ( present( file ) ) then
      this%file = file
    else
      this%file = ''
    end if

  end subroutine

  !======================================================
  ! Select
  !
  pure function select_integer ( table, cols ) result ( table_ )

    class(table_ty), intent(in) :: table
    integer,         intent(in) :: cols(:)
    type(table_ty)              :: table_

    call table_%init (                 &
      nrows    = table%nrows,          &
      ncols    = size(cols),           &
      rownames = table%rownames,       &
      colnames = table%colnames(cols), &
      cell     = table%cell(:, cols),  &
      name     = table%name,           &
      key      = table%key,            &
      file     = table%file )

  end function

  pure function select_logical ( table, cols ) result ( table_ )

    class(table_ty), intent(in) :: table
    logical,         intent(in) :: cols(:)
    type(table_ty)              :: table_
    integer, allocatable        :: jj(:)
    integer j

    jj = [( j, j = 1, size(cols) )]

    table_ = select_integer( table, pack( jj, cols ) )

  end function

  pure function select_character ( table, cols ) result ( table_ )

    class(table_ty), intent(in) :: table
    character(*),    intent(in) :: cols(:)
    type(table_ty)              :: table_
    integer                     :: jj(size(cols))
    integer j

    do concurrent ( j = 1:size(cols) )
      jj(j) = findloc( adjustl(table%colnames), cols(j), dim = 1 )
    end do

    table_ = select_integer( table, jj )

  end function

  !======================================================
  ! Filter
  !
  pure function filter_integer ( table, rows ) result ( table_ )

    class(table_ty), intent(in) :: table
    integer,         intent(in) :: rows(:)
    type(table_ty)              :: table_

    call table_%init(                  &
      nrows    = size(rows),           &
      ncols    = table%ncols,          &
      rownames = table%rownames(rows), &
      colnames = table%colnames,       &
      cell     = table%cell(rows, :),  &
      name     = table%name,           &
      key      = table%key,            &
      file     = table%file )

  end function

  pure function filter_logical ( table, rows ) result ( table_ )

    class(table_ty), intent(in) :: table
    logical,         intent(in) :: rows(:)
    type(table_ty)              :: table_
    integer, allocatable        :: ii(:)
    integer i

    ii = [( i, i = 1, size(rows) )]

    table_ = filter_integer( table, pack( ii, rows ) )

  end function

  pure function filter_character ( table, rows ) result ( table_ )

    class(table_ty), intent(in) :: table
    character(*),    intent(in) :: rows(:)
    type(table_ty)              :: table_
    integer                     :: ii(size(rows))
    integer i

    do concurrent ( i = 1:size(rows) )
      ii(i) = findloc( adjustl(table%rownames), rows(i), dim = 1 )
    end do

    table_ = filter_integer( table, ii )

  end function

  !======================================================
  ! Delete
  !

  pure function delete_integer ( table, rows ) result ( table_ )

    class(table_ty), intent(in) :: table
    integer,         intent(in) :: rows(:)
    type(table_ty)              :: table_
    integer, allocatable        :: ii(:)
    logical, allocatable        :: ll(:)
    integer i, j

    ii = [( i,      i = 1, table%nrows )]
    ll = [( .true., j = 1, table%nrows )]

    do i = 1, size(rows)
      j = findloc( ii, rows(i), dim = 1 )
      if ( j /= 0 ) then
        ll(j) = .false.
      end if
    end do

    table_ = filter_logical( table, ll )

  end function

  pure function delete_logical ( table, rows ) result ( table_ )

    class(table_ty), intent(in) :: table
    logical,         intent(in) :: rows(:)
    type(table_ty)              :: table_
    integer, allocatable        :: ii(:)
    integer i

    ii = [( i, i = 1, size(rows) )]

    table_ = delete_integer( table, pack( ii, rows ) )

  end function

  pure function delete_character ( table, rows ) result ( table_ )

    class(table_ty), intent(in) :: table
    character(*),    intent(in) :: rows(:)
    type(table_ty)              :: table_
    integer                     :: ii(size(rows))
    integer i

    do concurrent ( i = 1:size(rows) )
      ii(i) = findloc( adjustl(table%rownames), rows(i), dim = 1 )
    end do

    table_ = delete_integer( table, ii )

  end function

  !======================================================
  ! Insert Column
  !

  function insert_col ( table, x, colname, before ) result ( table_ )

    class(table_ty),        intent(in) :: table
    class(*),               intent(in) :: x(table%nrows)
    class(*),     optional, intent(in) :: before
    character(*), optional, intent(in) :: colname
    character(LEN_C)                   :: colname_
    character(LEN_C)                   :: c(table%nrows)
    integer                            :: before_
    type(table_ty)                     :: table_
    integer i

    if ( present( colname ) ) then
      colname_ = colname
    else
      colname_ = 'new'
    end if

    if ( present( before ) ) then
      select type ( before )
        type is ( character(*) )
          before_ = findloc( table%colnames, before, dim = 1 )
        type is ( integer )
          before_ = before
      end select
    else
      before_ = table%ncols + 1
    end if

    call table_%init(            &
      nrows    = table%nrows,     &
      ncols    = table%ncols + 1, &
      rownames = table%rownames,  &
      name     = table%name,      &
      key      = table%key,       &
      file     = table%file )

    select type ( x )
      type is ( character(*) )
        c = x
      type is ( logical )
        do concurrent ( i = 1:table%nrows )
          write ( c(i), '(l)' ) x(i)
        end do
      type is ( integer )
        do concurrent ( i = 1:table%nrows )
          write ( c(i), '(i0)' ) x(i)
        end do
      type is ( real )
        do concurrent ( i = 1:table%nrows )
          write ( c(i), '(g0)' ) x(i)
        end do
    end select

    if ( before_ == 1 ) then
      table_%cell(:, 1) = c
      table_%cell(:, 2:) = table%cell
      table_%colnames(1) = colname_
      table_%colnames(2:) = table%colnames
    else if ( before_ == table%ncols + 1 ) then
      table_%cell(:, 1:table%ncols) = table%cell
      table_%cell(:, table_%ncols) = c
      table_%colnames(1:table%ncols) = table%colnames
      table_%colnames(table%ncols + 1) = colname_
    else
      table_%cell(:, 1:before_ - 1) = table%cell(:, 1:before_ - 1)
      table_%cell(:, before_) = c
      table_%cell(:, before_ + 1:) = table%cell(:, before_:)
      table_%colnames(1:before_ - 1) = table%colnames(1:before_ - 1)
      table_%colnames(before_) = colname_
      table_%colnames(before_ + 1:) = table%colnames(before_:)
    end if

  end function

  !======================================================
  ! Join Functions
  !

  function inner_join ( table1, table2 ) result ( table3 )

    class(table_ty), intent(in) :: table1
    type(table_ty),  intent(in) :: table2
    type(table_ty)              :: table3
    integer i, i1, i2, j1, j2, u, nrows

    ! Find key columns
    j1 = findloc( adjustl(table1%colnames), table1%key, dim = 1 )
    j2 = findloc( adjustl(table2%colnames), table2%key, dim = 1 )

    open ( newunit = u, status = 'scratch', form = 'unformatted' )

    associate ( key1 => table1%cell(:, j1), &
                key2 => table2%cell(:, j2) )

    nrows = 0
    do i1 = 1, table1%nrows
      do i2 = 1, table2%nrows
        if ( key1(i1) == key2(i2) ) then
          write ( u ) table1%cell(i1, :), table2%cell(i2, :) 
          nrows = nrows + 1
        end if
      end do
    end do

    end associate

    rewind ( u )

    call table3%init( nrows    = nrows,                                &
                      ncols    = table1%ncols + table2%ncols,          &
                      colnames = [ table1%colnames, table2%colnames ], &
                      name     = table1%name,                          &
                      key      = table1%key,                           &
                      file     = table1%file )

    do i = 1, table3%nrows
      read ( u ) table3%cell(i, :)
    end do

    close ( u )

  end function

  function left_join ( table1, table2 ) result ( table3 )

    class(table_ty), intent(in) :: table1
    type(table_ty),  intent(in) :: table2
    type(table_ty)              :: table3
    logical                     :: found
    integer k, i1, i2, j1, j2, nrows

    ! Find key columns
    j1 = findloc( adjustl(table1%colnames), table1%key, dim = 1 )
    j2 = findloc( adjustl(table2%colnames), table2%key, dim = 1 )

    associate ( key1 => table1%cell(:, j1), &
                key2 => table2%cell(:, j2) )

    nrows = 0
    do i1 = 1, table1%nrows
      found = .false.
      do i2 = 1, table2%nrows
        if ( key1(i1) == key2(i2) ) then
          nrows = nrows + 1
          found = .true.
        end if
      end do
      if ( .not. found ) then
        nrows = nrows + 1
      end if
    end do

    call table3%init( nrows    = nrows,                                &
                      ncols    = table1%ncols + table2%ncols,          &
                      colnames = [ table1%colnames, table2%colnames ], &
                      name     = table1%name,                          &
                      key      = table1%key,                           &
                      file     = table1%file )
    table3%cell = 'NA'

    k = 1
    do i1 = 1, table1%nrows
      found = .false.
      do i2 = 1, table2%nrows
        if ( key1(i1) == key2(i2) ) then
          found = .true.
          table3%cell(k, :) = [ table1%cell(i1, :), table2%cell(i2, :) ]
          k = k + 1
        end if
      end do
      if ( .not. found ) then
        table3%cell(k, 1:table1%ncols) = table1%cell(i1, 1:table1%ncols)
        k = k + 1
      end if
    end do

    end associate

  end function

  function right_join ( table1, table2 ) result ( table3 )

    class(table_ty), intent(in) :: table1
    type(table_ty),  intent(in) :: table2
    type(table_ty)              :: table3_
    type(table_ty)              :: table3

    table3 = left_join( table2, table1 )

    table3_ = table3

    table3%colnames = [ table1%colnames, table2%colnames ]
    table3%cell(:, 1:table1%ncols)  = table3_%cell(:, table2%ncols+1:)
    table3%cell(:, table1%ncols+1:) = table3_%cell(:, 1:table2%ncols)

  end function

  pure function inner_join_pure ( table1, table2 ) result ( table3 )
  ! Note.
  ! Use inner_join (impure) function instead
  ! if table sizes are large and calculation is slow.

    class(table_ty), intent(in) :: table1
    type(table_ty),  intent(in) :: table2
    type(table_ty)              :: table3
    logical                     :: matches(table1%nrows, table2%nrows) ! can be very large
    integer i1, i2, k, j1, j2

    ! Find key columns
    j1 = findloc( adjustl(table1%colnames), table1%key, dim = 1 )
    j2 = findloc( adjustl(table2%colnames), table2%key, dim = 1 )

    associate ( key1 => table1%cell(:, j1), &
                key2 => table2%cell(:, j2) )

    ! Find matching combinations
    matches = .false.
    do concurrent ( i1 = 1:table1%nrows, i2 = 1:table2%nrows )
      if ( key1(i1) == key2(i2) ) then
        matches(i1, i2) = .true.
      end if
      !print *, 'i1: ', i1, ', i2: ', i2, 'key1(i1): ', key1(i1), 'key2(i2): ', key2(i2), ', matches: ', matches(i1, i2)
    end do

    end associate

    call table3%init( nrows    = count(matches),                       &
                      ncols    = table1%ncols + table2%ncols,          &
                      colnames = [ table1%colnames, table2%colnames ], &
                      name     = table1%name,                          &
                      key      = table1%key,                           &
                      file     = table1%file )
    k = 0
    do i1 = 1, table1%nrows
      do i2 = 1, table2%nrows
        if ( matches(i1, i2) ) then
          k = k + 1
          table3%cell(k, :) = [ table1%cell(i1, :), table2%cell(i2, :) ]
        end if
      end do
    end do

  end function inner_join_pure

  pure function left_join_pure ( table1, table2 ) result ( table3 )
  ! Note.
  ! Use left_join (impure) function instead
  ! if table sizes are large and calculation is slow.

    class(table_ty), intent(in) :: table1
    type(table_ty),  intent(in) :: table2
    type(table_ty)              :: table3
    logical                     :: matches(table1%nrows, table2%nrows) ! can be very large
    logical                     :: found
    integer i1, i2, j1, j2, k

    ! Find key columns
    j1 = findloc( adjustl(table1%colnames), table1%key, dim = 1 )
    j2 = findloc( adjustl(table2%colnames), table2%key, dim = 1 )

    associate ( key1 => table1%cell(:, j1), &
                key2 => table2%cell(:, j2) )

    ! Find matching combinations
    matches = .false.
    do concurrent ( i1 = 1:table1%nrows, i2 = 1:table2%nrows )
      if ( key1(i1) == key2(i2) ) then
        matches(i1, i2) = .true.
      end if
    end do

    end associate

    call table3%init( nrows    = count(matches) + table1%nrows,        &
                      ncols    = table1%ncols + table2%ncols,          &
                      colnames = [ table1%colnames, table2%colnames ], &
                      name     = table1%name,                          &
                      key      = table1%key,                           &
                      file     = table1%file )
    k = 0
    do i1 = 1, table1%nrows
      found = .false.
      do i2 = 1, table2%nrows
        if ( matches(i1, i2) ) then
          found = .true.
          k = k + 1
          table3%cell(k, :) = [ table1%cell(i1, :), table2%cell(i2, :) ]
        end if
      end do
      if ( .not. found ) then
        k = k + 1
        table3%cell(k, 1:table1%ncols)  = table1%cell(i1, :)
        table3%cell(k, table1%ncols+1:) = cNA
      end if
    end do

    table3%nrows = k
    table3%cell  = table3%cell(1:k, :)

  end function left_join_pure

  pure function right_join_pure ( table1, table2 ) result ( table3 )
  ! Note.
  ! Use right_join (impure) function instead
  ! if table sizes are large and calculation is slow.

    class(table_ty), intent(in) :: table1
    type(table_ty),  intent(in) :: table2
    type(table_ty)              :: table3_
    type(table_ty)              :: table3

    table3 = left_join_pure ( table2, table1 )

    table3_ = table3

    table3%colnames = [ table1%colnames, table2%colnames ]
    table3%cell(:, 1:table1%ncols)  = table3_%cell(:, table2%ncols+1:)
    table3%cell(:, table1%ncols+1:) = table3_%cell(:, 1:table2%ncols)

  end function right_join_pure

  pure function append ( table1, table2 ) result ( table3 )

    class(table_ty), intent(in) :: table1
    type(table_ty),  intent(in) :: table2
    type(table_ty)              :: table3

    call table3%init( nrows    = table1%nrows + table2%nrows, &
                      ncols    = table1%ncols,                &
                      colnames = table1%colnames,             &
                      name     = table1%name,                 &
                      key      = table1%key,                  &
                      file     = table1%file )

    table3%cell(1:table1%nrows,  :) = table1%cell
    table3%cell(table1%nrows+1:, :) = table2%cell

  end function append

  ! Insert or replace table2 into table1
  pure function insert_or_replace ( table1, table2 ) result ( table3 )

    class(table_ty),        intent(in) :: table1
    type(table_ty),         intent(in) :: table2
    type(table_ty)                     :: table3
    integer i1, i2, j_key, k

    ! Find key columns (shall be the same column index for both tables)
    j_key = findloc( adjustl(table1%colnames), table1%key, dim = 1 )

    !if ( j_key == 0 ) then
    !  print *, '*** Errro: table1 key', trim(table1%key), ' not found in colnames: ', table1%colnames
    !end if
    ! If key is not found, then the following error occurs:
    ! Subscript #2 of the array CELL has value 0 which is less than the lower bound of 1
    associate ( key1 => table1%cell(:, j_key), &
                key2 => table2%cell(:, j_key) )

    call table3%init( nrows    = size(union ( key1, key2 )), &
                      ncols    = table1%ncols,               &
                      colnames = table1%colnames,            &
                      name     = table1%name,                &
                      key      = table1%key,                 &
                      file     = table1%file )

    table3%cell(1:table1%nrows, :) = table1%cell

    k = table1%nrows + 1

    do i2 = 1, table2%nrows

      ! Replace
      do i1 = 1, table1%nrows
        if ( key1(i1) == key2(i2) ) then
          table3%cell(i1, :) = table2%cell(i2, :)
          exit
        end if
      end do

      ! Append
      if ( i1 > table1%nrows ) then
        table3%cell(k, :) = table2%cell(i2, :)
        k = k + 1
      end if

    end do

    end associate

  end function insert_or_replace

  !======================================================
  ! File I/O
  !

  subroutine write_csv ( this, file )

    class(table_ty), intent(in) :: this
    character(*),    intent(in) :: file
    integer i, u

    open ( newunit = u, file = file, status = 'unknown' )

    write ( u, '(a)' ) get_csvline_from_cells( this%colnames )

    do i = 1, this%nrows
      write ( u, '(a)' ) get_csvline_from_cells( this%cell(i, :) )
    end do

    close ( u )

  end subroutine write_csv

  subroutine write_parquet ( this, file, print )

    class(table_ty), intent(in) :: this
    character(*),    intent(in) :: file
    character(:), allocatable   :: csv
    logical, optional           :: print
    logical                     :: print_

    if ( present( print ) ) then
      print_ = print
    else
      print_ = .false.
    end if

    csv = file(1:len_trim(file)-8)//'.csv'
    call write_csv( this, csv )
    call csv2parquet( csv, file )
    if ( print_ ) then
      call execute_command_line( 'duckdb -c "SELECT * FROM '//"'"//trim(file)//"'"//'"' )
    end if

  end subroutine write_parquet

  subroutine csv2parquet ( csv, parquet )

    ! Note. duckdb is required

    character(*), intent(in)  :: csv, parquet
    character(:), allocatable :: query

    ! Do not include TIMESTAMP-related column types,
    ! or duckdb automatically converts datetime to UTC datatime.
    query = "COPY (SELECT * FROM read_csv('"//trim(csv)//&
          "', auto_type_candidates = ['BOOLEAN', 'BIGINT', 'DOUBLE', 'VARCHAR'])) TO '"//&
          trim(parquet)//"' (FORMAT 'parquet')"
    !print *, 'query: ', trim(query)
    call execute_command_line( 'duckdb -c "'//trim(query)//'"' )

  end subroutine csv2parquet

  subroutine read_csv ( this, file, stat )

    class(table_ty),   intent(inout) :: this
    character(*),      intent(in)    :: file
    integer, optional, intent(out)   :: stat
    character(LEN_C*100)             :: csvline
    character(255)                   :: iomsg
    integer(8)                       :: size
    logical                          :: exist
    integer i, u, nrows, ncols, iostat

    !print *, '[table_mo.f90:read_csv] Note. the maximum length of column string: ', LEN_C

    if ( present( stat ) ) stat = 0

    inquire( file = file, exist = exist, size = size )

    if ( .not. exist ) then
      print *, '[table_mo.f90:read_csv] *** Erorr: '//trim(file)//' does not exist.'
      if ( present( stat ) ) then
        stat = 1
        return
      else
        error stop 1
      end if
    end if

    if ( size < 2 ) then
      print *, '[table_mo.f90:read_csv] *** Erorr: '//trim(file)//' exists, however, empty.'
      if ( present( stat ) ) then
        stat = 1
        return
      else
        error stop 1
      end if
    end if

    open ( newunit = u, file = file, status = 'old', iomsg = iomsg, iostat = iostat )

    if ( iostat /= 0 ) then
      print *, '[table_mo.f90:read_csv] *** Erorr: '//trim(iomsg)
      if ( present( stat ) ) then
        stat = 1
        return
      else
        error stop 1
      end if
    end if

    nrows = count_rows( u ) - 1
    ncols = count_cols( u )

!    print *, 'nrows: ', nrows
!    print *, 'ncols: ', ncols
    if ( nrows < 1 ) then
      print *, '*** Error: no record found'
    end if

    call this%init( nrows = nrows, ncols = ncols, file = file )

    if ( nrows == 0 ) then
      print *, '[table_mo.f90:read_csv] *** Warning: No record is available. file: '//trim(file)
      if ( present( stat ) ) stat = 2
      return
    end if

    ! Colnames
    read ( u, '(a)' ) csvline
!    print *, 'colnames: ', trim(csvline)
    this%colnames = get_cells_from_csvline( csvline )
    this%key = this%colnames(1)

    ! Records
    do i = 1, nrows
      read ( u, '(a)' ) csvline
!      print *, 'csvline: ', trim(csvline)
      if ( count_seps( csvline ) + 1 /= ncols ) then
        print *, '[table_mo.f90:read_csv] *** Error: Irregular number of columns in record.'
        print *, '[table_mo.f90:read_csv] Filename: ', trim(file)
        print *, '[table_mo.f90:read_csv] # of delimiters: ', count_seps( csvline )
        print *, '[table_mo.f90:read_csv] # of columns: ', ncols
        print *, '[table_mo.f90:read_csv] Abort reading records.'
        print *, 'Line #: ', i, ', Record: ', trim(csvline)
        if ( present( stat ) ) stat = 3
        exit
      end if
      this%cell(i, 1:ncols) = get_cells_from_csvline( csvline )
    end do

    close ( u )

  end subroutine read_csv

  pure elemental function count_seps ( line, sep ) result ( n )
    character(*),           intent(in) :: line
    character(1), optional, intent(in) :: sep
    character(1)                       :: sep_
    integer i, n

    if ( present( sep ) ) then
      sep_ = sep
    else
      sep_ = ','
    end if
    n = 0
    do i = 1, len_trim(line)
      if ( line(i:i) /= sep_ ) cycle
      n = n + 1
    end do
  end function

  function count_cols ( u ) result ( ncols )
    integer, intent(in) :: u
    integer             :: ncols
    integer             :: iostat
    character(1)        :: chr
    ncols = 1
    rewind( u )
    do
      read ( u, '(a1)', advance = 'no', eor = 10, iostat = iostat ) chr
      if ( iostat /= 0 ) then
        ncols = 0
        return
      end if
      if ( chr == ',' ) ncols = ncols + 1
    end do
    10 rewind( u )
  end function count_cols

  function count_rows ( u ) result ( nrows )
    integer, intent(in) :: u
    integer             :: nrows
    integer             :: iostat
    nrows = 0
    rewind ( u )
    do
      read ( u, '()', end = 10, iostat = iostat )
      if ( iostat /= 0 ) then
        nrows = 0
        return
      end if
      nrows = nrows + 1
    end do
    10 rewind( u )
  end function count_rows

  pure function get_cells_from_csvline ( csvline ) result ( cells ) 

    character(*), intent(in)      :: csvline
    character(LEN_C), allocatable :: cells(:)
    integer, allocatable          :: p(:) ! Comma position
    integer len_line, i, nc

    len_line = len_trim(csvline)

    if ( len_line == 0 ) return

    allocate( p(len_line) ) ! Max: all commas in a line

    ! Get positions of commas in a line
    nc = 0
    do i = 1, len_line
      if ( csvline(i:i) /= ',' ) cycle
      nc = nc + 1
      p(nc) = i
    end do

    if ( allocated( cells ) ) deallocate( cells )

    if ( nc == 0 ) then
      allocate( cells(1) )
      cells = trim(csvline)
      return
    else
      allocate( cells(nc + 1) )
    end if

    ! Fetch a string between commas
    cells = ''
    cells(1)      = csvline(1:p(1) - 1)
    cells(nc + 1) = csvline(p(nc) + 1:len_line)

    do concurrent ( i = 1:nc - 1 )
      cells(i + 1) = csvline(p(i) + 1:p(i + 1) - 1)
    end do

    cells = adjustl(cells)

  end function get_cells_from_csvline

  pure function get_csvline_from_cells ( cells, sep ) result ( csvline )

    character(*), intent(in)           :: cells(:)
    character(1), intent(in), optional :: sep
    character(1)                       :: sep_
    character(:), allocatable          :: csvline
    integer j

    if ( present( sep ) ) then
      sep_ = sep
    else
      sep_ = ','
    end if

    csvline = trim(adjustl(cells(1)))

    do j = 2, size(cells)
      csvline = trim(csvline)//sep_//trim(adjustl(cells(j)))
    end do

  end function get_csvline_from_cells

  subroutine print_table ( this, n, len_cell )

    class(table_ty),   intent(in) :: this
    integer, optional, intent(in) :: n
    integer, optional, intent(in) :: len_cell
    integer                       :: len_cell_
    integer, parameter            :: LEN_DEFAULT = 23 ! Timestamp + UTC
    character(30)                 :: fmt_col, fmt_cell
    integer i, j

    if ( present( len_cell ) ) then
      len_cell_ = len_cell
    else
      len_cell_ = LEN_DEFAULT
    end if

    write ( fmt_col,  '(a, i0, a, i0, a)' ) "( ", this%ncols, "a", len_cell_, " )"
    write ( fmt_cell, '(a, i0, a, i0, a)' ) "( ", this%ncols, "a", len_cell_, ", '... ', i0 )"
!    print *, 'fmt_cell: ', trim(fmt_cell)

    print *, repeat('=', 79)
    if ( this%name /= '' ) print *, 'name : ', trim(this%name)
    if ( this%file /= '' ) print *, 'file : ', trim(this%file)
    print *, 'nrows:', this%nrows
    print *, 'ncols:', this%ncols

    print *, repeat('=', 79)
    print fmt_col, [( this%colnames(j), j = 1, this%ncols )]
    print *, repeat('-', 79)

    if ( present( n ) ) then
      do i = 1, min(n, this%nrows)
        print fmt_cell, [( this%cell(i, j), j = 1, this%ncols )], i
      end do
    else
      if ( this%nrows <= 15 ) then
        do i = 1, this%nrows
          print fmt_cell, [( this%cell(i, j), j = 1, this%ncols )], i
        end do
      else
        do i = 1, 5
          print fmt_cell, [( this%cell(i, j), j = 1, this%ncols )], i
        end do
        print *, ':'
        print *, ':'
        do i = this%nrows - 4, this%nrows
          print fmt_cell, [( this%cell(i, j), j = 1, this%ncols )], i
        end do
      end if
    end if

    print *, repeat('-', 79)
    print *, ''

  end subroutine print_table

  !==============================================
  ! Utility Functions
  !
  pure integer function index_row ( this, row )
    class(table_ty), intent(in) :: this
    character(*),    intent(in) :: row
    index_row = findloc( this%rownames, row, dim = 1 )
  end function index_row

  pure integer function index_col ( this, col )
    class(table_ty), intent(in) :: this
    character(*),    intent(in) :: col
    index_col = findloc( this%colnames, col, dim = 1 )
  end function index_col

!  pure function union ( set1, set2 ) result ( cup )
!
!    character(LEN_C), intent(in)  :: set1(:)
!    character(LEN_C), intent(in)  :: set2(:)
!    character(LEN_C), allocatable :: set3(:)
!    character(LEN_C), allocatable :: cup(:)
!    logical, allocatable          :: duplicated(:)
!    integer i, j, n
!
!    set3 = [ set1, set2 ] ! character length shall be the same in both set1 and set2
!
!    n = size(set3)
!
!    allocate ( duplicated(n), source = .false. )
!
!    do i = 1, n
!      if ( .not. duplicated(i) ) then
!        do concurrent ( j = 1:n, j /= i )
!          if ( set3(i) == set3(j) ) then
!            duplicated(j) = .true.
!          end if
!        end do
!      end if
!    end do
!
!    cup = pack( set3, .not. duplicated )
!
!  end function union

  pure function union ( set1, set2 ) result ( cup )

    character(LEN_C), intent(in)  :: set1(:)
    character(LEN_C), intent(in)  :: set2(:)
    character(LEN_C), allocatable :: cup(:)
    logical,          allocatable :: dup(:)
    integer i1, i2, n1, n2, n, ndups

    n1 = size(set1)
    n2 = size(set2)

    allocate( dup(n2), source = .false. )

    do concurrent ( i2 = 1:n2 )
      do concurrent ( i1 = 1:n1 )
        if ( set1(i1) == set2(i2) ) then
            dup(i2) = .true.
        end if
      end do
    end do

    ndups = count( dup )

    if ( ndups == 0 ) then
      cup = [ set1, set2 ]
    else
      n = n1 + n2 - ndups
      allocate( cup(n) )
      cup(1:n1) = set1
      cup(n1+1:n) = pack( set2, .not. dup )
    end if

  end function union

  pure function intersect ( set1, set2 ) result ( cap )

    character(LEN_C), intent(in)  :: set1(:)
    character(LEN_C), intent(in)  :: set2(:)
    character(LEN_C), allocatable :: cap(:)
    logical, allocatable          :: duplicated(:)
    integer i1, i2, n1, n2

    n1 = size(set1)
    n2 = size(set2)

    if ( n1 < n2 ) then ! set1 is smaller

      allocate ( duplicated(n1), source = .false. )

      do concurrent ( i1 = 1:n1 )
        do i2 = 1, n2
          if ( set1(i1) == set2(i2) ) then
            duplicated(i1) = .true.
            exit
          end if
        end do
      end do

      cap = pack( set1, duplicated )

    else ! set2 is smaller

      allocate( duplicated(n2), source = .false. )

      do concurrent ( i2 = 1:n2 )
        do i1 = 1, n1
          if ( set2(i2) == set1(i1) ) then
            duplicated(i2) = .true.
            exit
          end if
        end do
      end do

      cap = pack( set2, duplicated )

    end if

  end function intersect

  pure subroutine sort_character ( keys, ii )
    ! key shall have numeric value in every string

    character(*), intent(in)    :: keys(:) ! The array to be sorted
    integer,      intent(inout) :: ii(:)   ! The indices to track sorted order
    character(:), allocatable   :: str
    integer(8)                  :: a(size(keys)) ! Use int8 to handle datetime integer
    integer i, p, i_a

    do concurrent ( i = 1:size(keys) )
      str = ''
      do p = 1, len_trim(keys(i))
        i_a = iachar(keys(i)(p:p)) ! extracts character number from string
        if ( 48 <= i_a .and. i_a <= 57 ) then
          str = str//keys(i)(p:p)
        end if
      end do
      !print *, 'str:', trim(str)
      read ( str, * ) a(i)
    end do

    call sort_integer8( a, ii, 1, size(a) )

  end subroutine sort_character

  pure recursive subroutine sort_integer8 ( a, ii, low, high )
    integer(8), intent(inout) :: a(:)  ! The array to be sorted
    integer,    intent(inout) :: ii(:) ! The indices to track sorted order
    integer,    intent(in)    :: low, high
    integer i, pivot
    if ( low < high ) then
      call partition( a, ii, low, high, pivot )
      ! Sort the partitions concurrently
      do concurrent ( i = 1:2 )
        select case ( i )
        case ( 1 )
          call sort_integer8( a, ii, low, pivot - 1 )
        case ( 2 )
          call sort_integer8( a, ii, pivot + 1, high )
        end select
      end do
    end if
  contains
    pure subroutine partition ( a, ii, low, high, pivot )
      integer(8), intent(inout) :: a(:)      ! The array to partition
      integer,    intent(inout) :: ii(:)     ! The indices array
      integer,    intent(in)    :: low, high ! The range of the array
      integer,    intent(out)   :: pivot     ! The pivot index
      integer(8)                :: pivot_value, tmp_a
      integer i, j, tmp_ii
      pivot_value = a(high)
      i = low - 1
      do j = low, high - 1
        if ( a(j) <= pivot_value ) then
          i = i + 1
          tmp_a = a(i)
          a(i) = a(j)
          a(j) = tmp_a
          tmp_ii = ii(i)
          ii(i) = ii(j)
          ii(j) = tmp_ii
        end if
      end do
      tmp_a = a(i + 1)
      a(i + 1) = a(high)
      a(high) = tmp_a
      tmp_ii = ii(i + 1)
      ii(i + 1) = ii(high)
      ii(high) = tmp_ii
      pivot = i + 1
    end subroutine
  end subroutine sort_integer8

  pure recursive subroutine sort_integer4 ( a, ii, low, high )
    integer(4), intent(inout) :: a(:)  ! The array to be sorted
    integer,    intent(inout) :: ii(:) ! The indices to track sorted order
    integer,    intent(in)    :: low, high
    integer i, pivot
    if ( low < high ) then
      call partition( a, ii, low, high, pivot )
      ! Sort the partitions concurrently
      do concurrent ( i = 1:2 )
        select case ( i )
        case ( 1 )
          call sort_integer4( a, ii, low, pivot - 1 )
        case ( 2 )
          call sort_integer4( a, ii, pivot + 1, high )
        end select
      end do
    end if
  contains
    pure subroutine partition ( a, ii, low, high, pivot )
      integer(4), intent(inout) :: a(:)      ! The array to partition
      integer,    intent(inout) :: ii(:)     ! The indices array
      integer,    intent(in)    :: low, high ! The range of the array
      integer,    intent(out)   :: pivot     ! The pivot index
      integer                :: pivot_value, tmp_a
      integer i, j, tmp_ii
      pivot_value = a(high)
      i = low - 1
      do j = low, high - 1
        if ( a(j) <= pivot_value ) then
          i = i + 1
          tmp_a = a(i)
          a(i) = a(j)
          a(j) = tmp_a
          tmp_ii = ii(i)
          ii(i) = ii(j)
          ii(j) = tmp_ii
        end if
      end do
      tmp_a = a(i + 1)
      a(i + 1) = a(high)
      a(high) = tmp_a
      tmp_ii = ii(i + 1)
      ii(i + 1) = ii(high)
      ii(high) = tmp_ii
      pivot = i + 1
    end subroutine
  end subroutine sort_integer4

  !==============================================
  ! Column Converter
  !

  !----------------------------------------------
  ! Column (character) to another type'
  !
  function to_character_colindex ( table, col ) result ( cvals )
    class(table_ty), intent(in) :: table
    integer,         intent(in) :: col
    character(LEN_C)            :: cvals(table%nrows) 
    cvals = table%cell(:, col)
  end function to_character_colindex

  function to_character_colname ( table, col ) result ( cvals )
    class(table_ty), intent(in) :: table
    character(*),    intent(in) :: col
    character(LEN_C)            :: cvals(table%nrows) 
    cvals = table%cell(:, findloc( adjustl(table%colnames), col, dim = 1 ))
  end function to_character_colname

  function to_logical_colindex ( table, col ) result ( lvals )
    class(table_ty), intent(in) :: table
    integer,         intent(in) :: col
    logical                     :: lvals(table%nrows) 
    integer i
    if ( any( table%cell(:, col) == 'NA' ) ) then
      print *, '[table_mo.f90:to_logical_colindex] *** Error: logical column shall not have NAs'
      print *, '[table_mo.f90:to_logical_colindex] Consider using integer as 0: .false., 1: .true. and NA: iNA(e.g., -999)'
      error stop 1
    end if
    do concurrent ( i = 1:table%nrows )
      read ( table%cell(i, col), * ) lvals(i)
    end do
  end function to_logical_colindex

  function to_logical_colname ( table, col ) result ( lvals )
    class(table_ty), intent(in) :: table
    character(*),    intent(in) :: col
    logical                     :: lvals(table%nrows) 
    integer i, j
    j = findloc( adjustl(table%colnames), col, dim = 1 )
    if ( any( table%cell(:, j) == 'NA' ) ) then
      print *, '[table_mo.f90:to_logical_colname] *** Error: logical column shall not have NAs'
      print *, '[table_mo.f90:to_logical_colname] Consider using integer as 0: .false., 1: .true. and NA: iNA(e.g., -999)'
      error stop 1
    end if
    do concurrent ( i = 1:table%nrows )
      read ( table%cell(i, j), * ) lvals(i)
    end do
  end function to_logical_colname

  function to_integer_colindex ( table, col ) result ( ivals )
    class(table_ty), intent(in)   :: table
    integer,         intent(in)   :: col
    character(LEN_C), allocatable :: cell(:, :)
    integer                       :: ivals(table%nrows) 
    integer i
    cell = table%cell
    if ( any( cell(:, col) == 'NA' ) ) then
      print *, '[table_mo.f90:to_integer_colindex] *** Warning: integer column has NAs'
      print *, '[table_mo.f90:to_integer_colindex] NAs are replaced by iNA(e.g., -999)'
      do concurrent ( i = 1:table%nrows )
        if ( cell(i, col) == 'NA' ) then
          write ( cell(i, col), * ) iNA
        end if
      end do
    end if
    do concurrent ( i = 1:table%nrows )
      read ( cell(i, col), * ) ivals(i)
    end do
  end function to_integer_colindex

  function to_integer_colname ( table, col ) result ( ivals )
    class(table_ty), intent(in)   :: table
    character(*),    intent(in)   :: col
    character(LEN_C), allocatable :: cell(:, :)
    integer                       :: ivals(table%nrows) 
    integer i, j
    cell = table%cell
    j = findloc( adjustl(table%colnames), col, dim = 1 )
    if ( any( cell(:, j) == 'NA' ) ) then
      print *, '[table_mo.f90:to_integer_colname] *** Warning: integer column has NAs'
      print *, '[table_mo.f90:to_integer_colname] NAs are replaced by iNA(e.g., -999)'
      do concurrent ( i = 1:table%nrows )
        if ( cell(i, j) == 'NA' ) then
          write ( cell(i, j), * ) iNA
        end if
      end do
    end if
    do concurrent ( i = 1:table%nrows )
      read ( cell(i, j), * ) ivals(i)
    end do
  end function to_integer_colname

  function to_real_colindex ( table, col ) result ( rvals )
    class(table_ty), intent(in)   :: table
    integer,         intent(in)   :: col
    character(LEN_C), allocatable :: cell(:, :)
    real                          :: rvals(table%nrows) 
    integer i
    cell = table%cell
    if ( any( cell(:, col) == 'NA' ) ) then
      print *, '[table_mo.f90:to_real_colindex] *** Warning: real column has NAs'
      print *, '[table_mo.f90:to_real_colindex] NAs are replaced by rNA(e.g., -999.0)'
      do concurrent ( i = 1:table%nrows )
        if ( cell(i, col) == 'NA' ) then
          write ( cell(i, col), * ) rNA
        end if
      end do
    end if
    do concurrent ( i = 1:table%nrows )
      read ( cell(i, col), * ) rvals(i)
    end do
  end function to_real_colindex

  function to_real_colname ( table, col ) result ( rvals )
    class(table_ty), intent(in)   :: table
    character(*),    intent(in)   :: col
    character(LEN_C), allocatable :: cell(:, :)
    real                          :: rvals(table%nrows) 
    integer i, j
    cell = table%cell
    j = findloc( adjustl(table%colnames), col, dim = 1 )
    if ( any( cell(:, j) == 'NA' ) ) then
      print *, '[table_mo.f90:to_real_colname] *** Warning: real column has NAs'
      print *, '[table_mo.f90:to_real_colname] NAs are replaced by rNA(e.g., -999.0)'
      do concurrent ( i = 1:table%nrows )
        if ( cell(i, j) == 'NA' ) then
          write ( cell(i, j), * ) rNA
        end if
      end do
    end if
    do concurrent ( i = 1:table%nrows )
      read ( cell(i, j), * ) rvals(i)
    end do
  end function to_real_colname

  !----------------------------------------------
  ! Another type to column (character)
  !

  subroutine from_character_colindex ( table, vals, col )
    class(table_ty), intent(inout) :: table
    character(*),    intent(in)    :: vals(table%nrows) 
    integer,         intent(in)    :: col
    table%cell(:, col) = vals
  end subroutine from_character_colindex

  pure subroutine from_character_colname ( table, vals, col )
    class(table_ty), intent(inout) :: table
    character(*),    intent(in)    :: vals(table%nrows) 
    character(*),    intent(in)    :: col
    table%cell(:, findloc( adjustl(table%colnames), col, dim = 1 )) = vals
  end subroutine from_character_colname

  pure subroutine from_logical_colindex ( table, vals, col )
    class(table_ty), intent(inout) :: table
    logical,         intent(in)    :: vals(table%nrows) 
    integer,         intent(in)    :: col
    integer i
    do concurrent ( i = 1:table%nrows )
      write ( table%cell(i, col), * ) vals(i)
    end do
  end subroutine from_logical_colindex

  pure subroutine from_logical_colname ( table, vals, col )
    class(table_ty), intent(inout) :: table
    logical,         intent(in)    :: vals(table%nrows)
    character(*),    intent(in)    :: col
    integer i, j
    j = findloc( adjustl(table%colnames), col, dim = 1 )
    do concurrent ( i = 1:table%nrows )
     write ( table%cell(i, j), * ) vals(i)
    end do
  end subroutine from_logical_colname

  pure subroutine from_integer_colindex ( table, vals, col )
    class(table_ty), intent(inout) :: table
    integer,         intent(in)    :: vals(table%nrows)
    integer,         intent(in)    :: col
    integer i
    do concurrent ( i = 1:table%nrows )
      write ( table%cell(i, col), * ) vals(i)
    end do
  end subroutine from_integer_colindex

  pure subroutine from_integer_colname ( table, vals, col )
    class(table_ty), intent(inout) :: table
    integer,         intent(in)    :: vals(table%nrows)
    character(*),    intent(in)    :: col
    integer i, j
    j = findloc( adjustl(table%colnames), col, dim = 1 )
    do concurrent ( i = 1:table%nrows )
     write ( table%cell(i, j), * ) vals(i)
    end do
  end subroutine from_integer_colname

  pure subroutine from_real_colindex ( table, vals, col )
    class(table_ty), intent(inout) :: table
    real,            intent(in)    :: vals(table%nrows)
    integer,         intent(in)    :: col
    integer i
    do concurrent ( i = 1:table%nrows )
      write ( table%cell(i, col), * ) vals(i)
    end do
  end subroutine from_real_colindex

  pure subroutine from_real_colname ( table, vals, col )
    class(table_ty), intent(inout) :: table
    real,            intent(in)    :: vals(table%nrows)
    character(*),    intent(in)    :: col
    integer i, j
    j = findloc( adjustl(table%colnames), col, dim = 1 )
    do concurrent ( i = 1:table%nrows )
     write ( table%cell(i, j), * ) vals(i)
    end do
  end subroutine from_real_colname

  elemental pure logical function is_eq ( x, ref )
    real, intent(in) :: x
    real, intent(in) :: ref
    is_eq = abs(x - ref) < epsilon(ref)
  end function

  elemental function is_numeric ( string )
  !function is_numeric ( string )
    CHARACTER(len=*), intent(in) :: string
    character(len(string))       :: str
    LOGICAL                      :: is_numeric
    REAL                         :: x
    integer                      :: e
    !integer                      :: digit
    !CHARACTER(len=12)            :: fmt
    str = trim( adjustl(string) )
    ! digit = len_trim(str)
    ! BN: https://www.jamstec.go.jp/es/jp/simschool/f90learning/chap3/page3.html
    ! Digits: n
    x = -999.0
    !write( fmt, '("(BN, F", I0, ".0)")' ) digit
    read ( str, *, iostat = e ) x
    !print *, 'e:', e, 'string: ', trim(string)
    !print *, 'digit: ', digit, ', fmt: ', fmt, ', value:', x
    !is_numeric = e == 0 .and. digit /= 0
    is_numeric = e == 0
    !if ( is_numeric ) then
    !  print *, 'is numeric: ', trim(string)
    !else
    !  print *, 'is NOT numeric: ', trim(string)
    !end if
  end function

  pure elemental function round ( val, n ) result ( rx )
    real,    intent(in) :: val
    integer, intent(in) :: n
    real :: rx
    rx = anint(val*10.0**n)/10.0**n
  end function

end module

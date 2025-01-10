program unit_test_table_mo

  use table_mo

  implicit none

  type(table_ty)                :: table, table1, table2, table3, table4
  character(LEN_C), allocatable :: set1(:), set2(:), set3(:), str(:)
  character(LEN_C), allocatable :: cvals(:)
  logical,          allocatable :: lvals(:)
  integer,          allocatable :: ivals(:)
  real,             allocatable :: rvals(:)
  integer, allocatable :: ii(:), jj(:)
  integer i

  print *, '=========================================='
  print *, 'Test 1: Table Initialization'
  print *, '=========================================='

  call table%init (                        &
    nrows    = 3,                          &
    ncols    = 3,                          &
    rownames = [ 'row1', 'row2', 'row3' ], &
    colnames = [ 'col1', 'col2', 'col3' ], &
    name     = 'table1',                   &
    file     = 'table1.csv' )

  ! Value assignment
  table%cell(1, :) = [ '11', '12', '13' ]
  table%cell(2, :) = [ '21', '22', '23' ]
  table%cell(3, :) = [ '31', '32', '33' ]

  call table%print
  call table%print ( n = 2 ) ! with n rows 

  print *, '=========================================='
  print *, 'Test 2: Select'
  print *, '=========================================='

  print *, '------------------------------------------'
  print *, 'Test 2-1: with column indeces'
  print *, '------------------------------------------'

  table1 = select ( table, [ 3, 2 ] ) 

  call table1%print

  print *, '------------------------------------------'
  print *, 'Test 2-2: with logical vector'
  print *, '------------------------------------------'

  table1 = select ( table, [ .false., .true., .false. ] ) 

  call table1%print

  print *, '------------------------------------------'
  print *, 'Test 2-3: with colnames'
  print *, '------------------------------------------'

  table1 = select ( table, ['col3', 'col2'] )

  call table1%print

  print *, '------------------------------------------'
  print *, 'Test 2-4: as type-bound procedure'
  print *, '------------------------------------------'

  table1 = table%select ( [ 'col3', 'col2' ] )

  call table1%print

  print *, '=========================================='
  print *, 'Test 3: Filter'
  print *, '=========================================='

  print *, '------------------------------------------'
  print *, 'Test 3-1: with row indeces'
  print *, '------------------------------------------'

  table1 = filter ( table, [ 3, 2 ] )

  call table1%print

  print *, '------------------------------------------'
  print *, 'Test 3-2: with logical vector'
  print *, '------------------------------------------'

  table1 = filter ( table, [ .true., .false., .true. ] )

  call table1%print

  print *, '------------------------------------------'
  print *, 'Test 3-3: with rownames'
  print *, '------------------------------------------'

  table1 = filter ( table, [ 'row3', 'row1' ] )

  call table1%print

  print *, '-----------------------------------------'
  print *, 'Test 3-3: as type-bound procedure'
  print *, '-----------------------------------------'

  table1 = table%filter ( [ .true., .false., .true. ] )

  call table1%print

  print *, '=========================================='
  print *, 'Test 4: Inner Join'
  print *, '=========================================='

  call table1%init ( nrows = 4, ncols = 3 )
  table1%colnames   = [ 'key    ', 't1_col2', 't1_col3' ]
  table1%cell(1, :) = [ 'key1 ',   't1_12',   't1_13'   ]
  table1%cell(2, :) = [ 'key2 ',   't1_22',   't1_23'   ]
  table1%cell(3, :) = [ 'key3 ',   't1_32',   't1_33'   ]
  table1%cell(4, :) = [ 'key2 ',   't1_42',   't1_43'   ] ! Duplicated key

  call table2%init ( nrows = 3, ncols = 3 )
  table2%colnames   = [ 't2_col1', 'key    ', 't2_col3' ]
  table2%cell(1, :) = [ 't2_11',   'key3 ',   't2_13'   ]
  table2%cell(2, :) = [ 't2_21',   'key2 ',   't2_23'   ]
  table2%cell(3, :) = [ 't2_31',   'key5 ',   't2_33'   ]

  print *, '-----------------------------------------'
  print *, 'Test 4-1: with function'
  print *, '-----------------------------------------'

  table3 = inner_join ( table1, table2 )

  call table3%print

  print *, '-----------------------------------------'
  print *, 'Test 4-2: as type-bound procedure'
  print *, '-----------------------------------------'

  table3 = table1%inner_join ( table2 )

  call table3%print

  print *, '-----------------------------------------'
  print *, 'Test 4-3: with pure function (slow with large tables)'
  print *, '-----------------------------------------'

  table3 = inner_join_pure ( table1, table2 )

  call table3%print

  print *, '=========================================='
  print *, 'Test 5: Left Join'
  print *, '=========================================='

  print *, '-----------------------------------------'
  print *, 'Test 5-1: with function'
  print *, '-----------------------------------------'

  table3 = left_join ( table1, table2 )

  call table3%print

  print *, '-----------------------------------------'
  print *, 'Test 5-2: as type-bound procedure'
  print *, '-----------------------------------------'

  table3 = table1%left_join ( table2 )

  call table3%print

  print *, '-----------------------------------------'
  print *, 'Test 5-3: with operator(*)'
  print *, '-----------------------------------------'

  table3 = table1 * table2

  call table3%print

  print *, '-----------------------------------------'
  print *, 'Test 5-4: with pure function (slow with large tables)'
  print *, '-----------------------------------------'

  table3 = left_join_pure ( table1, table2 )

  call table3%print

  print *, '=========================================='
  print *, 'Test 6: Right Join'
  print *, '=========================================='

  print *, '-----------------------------------------'
  print *, 'Test 6-1: with function'
  print *, '-----------------------------------------'

  table3 = right_join ( table1, table2 )

  call table3%print

  print *, '-----------------------------------------'
  print *, 'Test 6-2: as type-bound procedure'
  print *, '-----------------------------------------'

  table3 = table1%right_join ( table2 )

  call table3%print

  print *, '-----------------------------------------'
  print *, 'Test 6-3: with pure function (slow with large tables)'
  print *, '-----------------------------------------'

  table3 = right_join_pure ( table1, table2 )

  call table3%print

  print *, '=========================================='
  print *, 'Test 7: Insert or Replace'
  print *, '=========================================='

  ! key shall be unique for insert or replace operation
  call table1%init ( nrows = 3, ncols = 3 )
  table1%colnames   = [ 'key    ', 't1_col2', 't1_col3' ]
  table1%cell(1, :) = [ 'key1 ',   't1_12',   't1_13'   ]
  table1%cell(2, :) = [ 'key2 ',   't1_22',   't1_23'   ]
  table1%cell(3, :) = [ 'key3 ',   't1_32',   't1_33'   ]

  print *, '-----------------------------------------'
  print *, 'Test 7-1: with function'
  print *, '-----------------------------------------'

  table3 = insert_or_replace ( table1, table2 )

  call table3%print

  print *, '-----------------------------------------'
  print *, 'Test 7-2: as type-bound procedure'
  print *, '-----------------------------------------'

  table3 = table1%insert_or_replace ( table2 )

  call table3%print

  print *, '-----------------------------------------'
  print *, 'Test 7-3: with operator(+)'
  print *, '-----------------------------------------'

  table3 = table1 + table2

  call table3%print

  print *, '=========================================='
  print *, 'Test 8: Append'
  print *, '=========================================='

  print *, '-----------------------------------------'
  print *, 'Test 8-1: with function'
  print *, '-----------------------------------------'

  table3 = append ( table1, table2 )

  print *, '-----------------------------------------'
  print *, 'Test 8-2: as type-bound procedure'
  print *, '-----------------------------------------'

  table3 = table1%append ( table2 )

  call table3%print

  print *, '=========================================='
  print *, 'Test 9: File I/O'
  print *, '=========================================='

  print *, '-----------------------------------------'
  print *, 'Test 9-1: Write CSV File'
  print *, '-----------------------------------------'

  call table1%write_csv ( file = 'table1.csv' )
  call table2%write_csv ( file = 'table2.csv' )

  print *, '-----------------------------------------'
  print *, 'Test 9-2: Read CSV File'
  print *, '-----------------------------------------'

  call table3%read_csv ( file = 'table1.csv' )
  call table4%read_csv ( file = 'table2.csv' )

  print *, '=========================================='
  print *, 'Test 10: Column Converter'
  print *, '=========================================='

  call table%init (                        &
    nrows    = 3,                          &
    ncols    = 4,                          &
    rownames = [ 'row1', 'row2', 'row3' ], &
    colnames = [ 'col1', 'col2', 'col3', 'col4' ] )

  table%cell(1, :) = [ 'str1', 'T   ', '11  ', '1.2 ' ]
  table%cell(2, :) = [ 'str2', 'F   ', '21  ', '2.2 ' ]
  table%cell(3, :) = [ 'str3', 'T   ', '31  ', '3.2 ' ]

  call table%print

  print *, '---------------------------------------------'
  print *, 'Test 10-1: Column (character) to another type'
  print *, '---------------------------------------------'

  cvals = table%to_character ( col = 1 )
  print *, cvals

  cvals = table%to_character ( col = 'col1' )
  print *, cvals

  lvals = table%to_logical ( col = 2 )
  print *, lvals

  lvals = table%to_logical ( col = 'col2' )
  print *, lvals

  ivals = table%to_integer ( col = 3 )
  print *, ivals

  ivals = table%to_integer ( col = 'col3' )
  print *, ivals

  rvals = table%to_real ( col = 4 )
  print *, rvals

  rvals = table%to_real ( col = 'col4' )
  print *, rvals

  print *, '---------------------------------------------'
  print *, 'Test 10-2: Another type to column (character)'
  print *, '---------------------------------------------'

  table%cell = '' ! clear cells

  call table%from_character ( cvals, col = 1 )
  print *, table%cell(:, 1)

  call table%from_character ( cvals, col = 'col1' )
  print *, table%cell(:, 1)

  call table%from_logical ( lvals, col = 2 )
  print *, table%cell(:, 2)

  call table%from_logical ( lvals, col = 'col2' )
  print *, table%cell(:, 2)

  call table%from_integer ( ivals, col = 3 )
  print *, table%cell(:, 3)

  call table%from_integer ( ivals, col = 'col3' )
  print *, table%cell(:, 3)

  call table%from_real ( rvals, col = 4 )
  print *, table%cell(:, 4)

  call table%from_real ( rvals, col = 'col4' )
  print *, table%cell(:, 4)

  print *, '=========================================='
  print *, 'Test A: Utility Functions'
  print *, '=========================================='

  print *, '-----------------------------------------'
  print *, 'Test A-1: Union of 2 sets'
  print *, '-----------------------------------------'

  set1 = ['key1', 'key2', 'key3']
  set2 = ['key4', 'key1', 'key5', 'key2']

  set3 = union ( set1, set2 )

  do i = 1, size(set3)
    print *, i, trim(set3(i))
  end do

  print *, '-----------------------------------------'
  print *, 'Test A-2: Intersect of 2 sets'
  print *, '-----------------------------------------'

  set1 = ['key1', 'key2', 'key3']
  set2 = ['key4', 'key1', 'key5', 'key2']

  set3 = intersect ( set1, set2 )

  do i = 1, size(set3)
    print *, i, trim(set3(i))
  end do

  print *, '-----------------------------------------'
  print *, 'Test A-3: Integer quick sort with indeces'
  print *, '-----------------------------------------'

  jj = [10, 3, 1, -1, 0]
  ii  = [( i, i = 1, size(jj) )]

  call quicksort_integer ( jj, ii, 1, size(ii) )

  do i = 1, size(jj)
    print *, 'Rank:', i, ', Value: ', jj(i)
  end do

  print *, '-----------------------------------------'
  print *, 'Test A-4: String quick sort with indeces'
  print *, '-----------------------------------------'

  str = [ 'd12', 'c44', 'a31', 'b29' ]
  ii  = [( i, i = 1, size(str) )]

  call quicksort_string ( str, ii )

  do i = 1, size(str)
    print *, 'Rank:', i, ', Value: ', trim( str(ii(i)) )
  end do

end program

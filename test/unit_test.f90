program unit_test_table_mo

  use table_mo

  implicit none

  type(table_ty)                :: table, table1, table2, table3, table4
  type(table_ty)                :: table_empty
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

  call table%init(                         &
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
  call table%print( n = 2 ) ! with n rows 

  print *, '------------------------------------------'
  print *, 'Test 1-1: Empty table'
  print *, '------------------------------------------'

  call table_empty%init( &
    nrows    = 0,   & ! <- This means an empty table
    ncols    = 3,   &
    colnames = [ 'col1', 'col2', 'col3' ] )

  call table_empty%print

  print *, '=========================================='
  print *, 'Test 2: Select'
  print *, '=========================================='

  print *, '------------------------------------------'
  print *, 'Test 2-1: with column indices'
  print *, '------------------------------------------'

  table1 = select( table, [ 3, 2 ] ) 

  call table1%print

  print *, '------------------------------------------'
  print *, 'Test 2-2: with logical vector'
  print *, '------------------------------------------'

  table1 = select( table, [ .false., .true., .false. ] ) 

  call table1%print

  print *, '------------------------------------------'
  print *, 'Test 2-3: with colnames'
  print *, '------------------------------------------'

  table1 = select( table, ['col3', 'col2'] )

  call table1%print

  print *, '------------------------------------------'
  print *, 'Test 2-4: as type-bound procedure'
  print *, '------------------------------------------'

  table1 = table%select( [ 'col3', 'col2' ] )

  call table1%print

  print *, '=========================================='
  print *, 'Test 3: Filter'
  print *, '=========================================='

  print *, '------------------------------------------'
  print *, 'Test 3-1: with row indices'
  print *, '------------------------------------------'

  table1 = filter( table, [ 3, 2 ] )

  call table1%print

  print *, '------------------------------------------'
  print *, 'Test 3-2: with logical vector'
  print *, '------------------------------------------'

  table1 = filter( table, [ .true., .false., .true. ] )

  call table1%print

  print *, '------------------------------------------'
  print *, 'Test 3-3: with rownames'
  print *, '------------------------------------------'

  table1 = filter( table, [ 'row3', 'row1' ] )

  call table1%print

  print *, '-----------------------------------------'
  print *, 'Test 3-3: as type-bound procedure'
  print *, '-----------------------------------------'

  table1 = table%filter( [ .true., .false., .true. ] )

  call table1%print

  print *, '=========================================='
  print *, 'Test 4: Delete'
  print *, '=========================================='

  print *, '------------------------------------------'
  print *, 'Test 4-1: with row indices'
  print *, '------------------------------------------'

  table1 = delete( table, [ 3, 2 ] )

  call table1%print

  print *, '------------------------------------------'
  print *, 'Test 4-2: with logical vector'
  print *, '------------------------------------------'

  table1 = delete( table, [ .true., .false., .true. ] )

  call table1%print

  print *, '------------------------------------------'
  print *, 'Test 4-3: with rownames'
  print *, '------------------------------------------'

  table1 = delete( table, [ 'row3', 'row1' ] )

  call table1%print

  print *, '-----------------------------------------'
  print *, 'Test 4-4: as type-bound procedure'
  print *, '-----------------------------------------'

  table1 = table%delete( [ .true., .false., .true. ] )

  call table1%print

  print *, '=========================================='
  print *, 'Test 5: Insert Column'
  print *, '=========================================='

  print *, '------------------------------------------'
  print *, 'Test 5-1: with no option (default)'
  print *, '------------------------------------------'

  lvals = [( .true., i = 1, table%nrows )]
  table1 = table%insert_col( x = lvals )
  call table1%print

  ivals = [( i*2, i = 1, table%nrows )]
  table1 = table%insert_col( x = ivals )
  call table1%print

  rvals = [( real(i*2), i = 1, table%nrows )]
  table1 = table%insert_col( x = rvals )
  call table1%print

  cvals = [( 'char', i = 1, table%nrows )]
  table1 = table%insert_col( x = cvals )
  call table1%print

  print *, '------------------------------------------'
  print *, 'Test 5-2: option with col index'
  print *, '------------------------------------------'

  cvals = [( 'char', i = 1, table%nrows )]
  table1 = table%insert_col( x = cvals, before = 1 )
  call table1%print

  cvals = [( 'char', i = 1, table%nrows )]
  table1 = table%insert_col( x = cvals, before = 3 )
  call table1%print

  print *, '------------------------------------------'
  print *, 'Test 5-3: option with col name'
  print *, '------------------------------------------'

  cvals = [( 'char', i = 1, table%nrows )]
  table1 = table%insert_col( x = cvals, before = 'col2' )
  call table1%print

  print *, '------------------------------------------'
  print *, 'Test 5-3: option with col name'
  print *, '------------------------------------------'

  cvals = [( 'char', i = 1, table%nrows )]
  table1 = table%insert_col( x = cvals, colname = 'this_col' )
  call table1%print

  print *, '------------------------------------------'
  print *, 'Test 5-4: option with col name'
  print *, '------------------------------------------'

  cvals = [( 'char', i = 1, table%nrows )]
  table1 = table%insert_col( x = cvals, before = 'col3', colname = 'this_col' )
  call table1%print

  print *, '=========================================='
  print *, 'Test SQL1: Inner Join'
  print *, '=========================================='

  call table1%init( nrows = 4, ncols = 3 )
  table1%colnames   = [ 'key    ', 't1_col2', 't1_col3' ]
  table1%cell(1, :) = [ 'key1 ',   't1_12',   't1_13'   ]
  table1%cell(2, :) = [ 'key2 ',   't1_22',   't1_23'   ]
  table1%cell(3, :) = [ 'key3 ',   't1_32',   't1_33'   ]
  table1%cell(4, :) = [ 'key2 ',   't1_42',   't1_43'   ] ! Duplicated key

  ! Specify key column, otherwise the fisrt column is used as the key.
  call table2%init ( nrows = 3, ncols = 3, key = 'key' )
  table2%colnames   = [ 't2_col1', 'key    ', 't2_col3' ]
  table2%cell(1, :) = [ 't2_11',   'key3 ',   't2_13'   ]
  table2%cell(2, :) = [ 't2_21',   'key2 ',   't2_23'   ]
  table2%cell(3, :) = [ 't2_31',   'key5 ',   't2_33'   ]

  print *, '-----------------------------------------'
  print *, 'Test SQL1-1: with function'
  print *, '-----------------------------------------'

  table3 = inner_join( table1, table2 )

  call table3%print

  print *, '-----------------------------------------'
  print *, 'Test SQL1-2: as type-bound procedure'
  print *, '-----------------------------------------'

  table3 = table1%inner_join( table2 )

  call table3%print

  print *, '-----------------------------------------'
  print *, 'Test SQL1-3: with pure function (slow with large tables)'
  print *, '-----------------------------------------'

  table3 = inner_join_pure( table1, table2 )

  call table3%print

  print *, '=========================================='
  print *, 'Test SQL2: Left Join'
  print *, '=========================================='

  print *, '-----------------------------------------'
  print *, 'Test SQL2-1: with function'
  print *, '-----------------------------------------'

  table3 = left_join( table1, table2 )

  call table3%print

  print *, '-----------------------------------------'
  print *, 'Test SQL2-2: as type-bound procedure'
  print *, '-----------------------------------------'

  table3 = table1%left_join( table2 )

  call table3%print

  print *, '-----------------------------------------'
  print *, 'Test SQL2-3: with operator(*)'
  print *, '-----------------------------------------'

  table3 = table1 * table2

  call table3%print

  print *, '-----------------------------------------'
  print *, 'Test SQL2-4: with pure function (slow with large tables)'
  print *, '-----------------------------------------'

  table3 = left_join_pure( table1, table2 )

  call table3%print

  print *, '=========================================='
  print *, 'Test SQL3: Right Join'
  print *, '=========================================='

  print *, '-----------------------------------------'
  print *, 'Test SQL3-1: with function'
  print *, '-----------------------------------------'

  table3 = right_join( table1, table2 )

  call table3%print

  print *, '-----------------------------------------'
  print *, 'Test SQL3-2: as type-bound procedure'
  print *, '-----------------------------------------'

  table3 = table1%right_join( table2 )

  call table3%print

  print *, '-----------------------------------------'
  print *, 'Test SQL3-3: with pure function (slow with large tables)'
  print *, '-----------------------------------------'

  table3 = right_join_pure( table1, table2 )

  call table3%print

  print *, '=========================================='
  print *, 'Test SQL4: Insert or Replace'
  print *, '=========================================='

  ! key shall be unique for insert or replace operation
  call table1%init( nrows = 3, ncols = 3 )
  table1%colnames   = [ 'key    ', 't1_col2', 't1_col3' ]
  table1%cell(1, :) = [ 'key1 ',   't1_12',   't1_13'   ]
  table1%cell(2, :) = [ 'key2 ',   't1_22',   't1_23'   ]
  table1%cell(3, :) = [ 'key3 ',   't1_32',   't1_33'   ]

  print *, '-----------------------------------------'
  print *, 'Test SQL4-1: with function'
  print *, '-----------------------------------------'

  table3 = insert_or_replace( table1, table2 )

  call table3%print

  print *, '-----------------------------------------'
  print *, 'Test SQL4-2: as type-bound procedure'
  print *, '-----------------------------------------'

  table3 = table1%insert_or_replace( table2 )

  call table3%print

  print *, '-----------------------------------------'
  print *, 'Test SQL4-3: with operator(+)'
  print *, '-----------------------------------------'

  table3 = table1 + table2

  call table3%print

  print *, '-----------------------------------------'
  print *, 'Test SQL4-4: with operator(+) and empty table'
  print *, '-----------------------------------------'

  table3 = table1 + table_empty

  call table3%print

  print *, '=========================================='
  print *, 'Test SQL5: Append'
  print *, '=========================================='

  print *, '-----------------------------------------'
  print *, 'Test SQL5-1: with function'
  print *, '-----------------------------------------'

  table3 = append( table1, table2 )

  print *, '-----------------------------------------'
  print *, 'Test SQL5-2: as type-bound procedure'
  print *, '-----------------------------------------'

  table3 = table1%append( table2 )

  call table3%print

  print *, '-----------------------------------------'
  print *, 'Test SQL5-2: as type-bound procedure and empty table'
  print *, '-----------------------------------------'

  table3 = table1%append( table_empty )

  call table3%print

  print *, '=========================================='
  print *, 'Test A: File I/O'
  print *, '=========================================='

  print *, '-----------------------------------------'
  print *, 'Test A-1: Write CSV File'
  print *, '-----------------------------------------'

  call table1%write_csv( file = 'table1.csv' )
  call table2%write_csv( file = 'table2.csv' )
  call table_empty%write_csv( file = 'table_empty.csv' )

  print *, '-----------------------------------------'
  print *, 'Test A-2: Read CSV File'
  print *, '-----------------------------------------'

  call table3%read_csv( file = 'table1.csv' )
  call table4%read_csv( file = 'table2.csv' )

  print *, '-----------------------------------------'
  print *, 'Test A-3: Write Parquet File'
  print *, 'Note. duckdb is required'
  print *, '-----------------------------------------'

  call table1%write_parquet( file = 'table1.parquet' )
  call table2%write_parquet( file = 'table2.parquet', print = .true. )

  print *, '=========================================='
  print *, 'Test B: Column Converter'
  print *, '=========================================='

  call table%init(                         &
    nrows    = 3,                          &
    ncols    = 4,                          &
    rownames = [ 'row1', 'row2', 'row3' ], &
    colnames = [ 'col1', 'col2', 'col3', 'col4' ] )

  table%cell(1, :) = [ 'str1', 'T   ', 'NA  ', '1.2 ' ]
  table%cell(2, :) = [ 'NA  ', 'F   ', '21  ', 'NA  ' ]
  table%cell(3, :) = [ 'str3', 'T   ', '31  ', '3.2 ' ]

  call table%print

  print *, '---------------------------------------------'
  print *, 'Test B-1: Column (character) to another type'
  print *, '---------------------------------------------'

  cvals = table%to_character( col = 1 )
  print *, cvals

  cvals = table%to_character( col = 'col1' )
  print *, cvals

  lvals = table%to_logical( col = 2 )
  print *, lvals

  lvals = table%to_logical( col = 'col2' )
  print *, lvals

  ivals = table%to_integer( col = 3 )
  print *, ivals

  ivals = table%to_integer( col = 'col3' )
  print *, ivals

  rvals = table%to_real( col = 4 )
  print *, rvals

  rvals = table%to_real( col = 'col4' )
  print *, rvals

  ! Error check : logical column has NA's
  !table%cell(2, :) = [ 'str2', 'NA  ', '21  ', '2.2 ' ]
  !lvals = table%to_logical ( col = 2 )
  !lvals = table%to_logical ( col = 'col2' )
  !print *, lvals

  print *, '---------------------------------------------'
  print *, 'Test B-2: Another type to column (character)'
  print *, '---------------------------------------------'

  table%cell = '' ! clear cells

  call table%from_character( cvals, col = 1 )
  print *, table%cell(:, 1)

  call table%from_character( cvals, col = 'col1' )
  print *, table%cell(:, 1)

  call table%from_logical( lvals, col = 2 )
  print *, table%cell(:, 2)

  call table%from_logical( lvals, col = 'col2' )
  print *, table%cell(:, 2)

  call table%from_integer( ivals, col = 3 )
  print *, table%cell(:, 3)

  call table%from_integer( ivals, col = 'col3' )
  print *, table%cell(:, 3)

  call table%from_real( rvals, col = 4 )
  print *, table%cell(:, 4)

  call table%from_real( rvals, col = 'col4' )
  print *, table%cell(:, 4)

  print *, '=========================================='
  print *, 'Test C: Utility Functions'
  print *, '=========================================='

  print *, '-----------------------------------------'
  print *, 'Test C-1: Get index of row / col'
  print *, '-----------------------------------------'

  print *, 'Index of row:', table%index_row( row = 'row2' )
  print *, 'Index of col:', table%index_col( col = 'col3' )

  print *, '-----------------------------------------'
  print *, 'Test C-2: Union of 2 sets'
  print *, '-----------------------------------------'

  set1 = ['key1', 'key2', 'key3']
  set2 = ['key4', 'key1', 'key5', 'key2']

  set3 = union( set1, set2 )

  do i = 1, size(set3)
    print *, i, trim(set3(i))
  end do

  print *, '-----------------------------------------'
  print *, 'Test C-2a: Union of 2 sets of timestamps'
  print *, '-----------------------------------------'

  set1 = ['2000-01-01 00:00:00', '2000-01-01 00:00:01', '2009-01-01 00:00:01']
  set2 = ['2000-01-01 00:00:00', '2000-01-01 00:00:01', '2009-01-01 00:00:02']

  set3 = union( set1, set2 )

  do i = 1, size(set3)
    print *, i, trim(set3(i))
  end do

  print *, '-----------------------------------------'
  print *, 'Test C-3: Intersect of 2 sets'
  print *, '-----------------------------------------'

  set1 = ['key1', 'key2', 'key3']
  set2 = ['key4', 'key1', 'key5', 'key2']

  set3 = intersect( set1, set2 )

  do i = 1, size(set3)
    print *, i, trim(set3(i))
  end do

  print *, '-----------------------------------------'
  print *, 'Test C-4: Integer sort with indices'
  print *, '-----------------------------------------'

  jj = [10, 3, 1, -1, 0]
  ii  = [( i, i = 1, size(jj) )]

  call sort_integer( jj, ii, 1, size(ii) )

  do i = 1, size(jj)
    print *, 'Rank:', i, ', Value: ', jj(i)
  end do

  print *, '-----------------------------------------'
  print *, 'Test C-5: String sort with indices'
  print *, '-----------------------------------------'

  str = [ 'd12', 'c44', 'a31', 'b29' ]
  ii  = [( i, i = 1, size(str) )]

  call sort_character( str, ii )

  do i = 1, size(str)
    print *, 'Rank:', i, ', Value: ', trim( str(ii(i)) )
  end do

  print *, '-----------------------------------------'
  print *, 'Test C-6: Datetime sort with indices'
  print *, '-----------------------------------------'

  str = [ '2025-05-01 20:00:00', '2025-05-01 12:00:00', '2025-05-01 06:00:00', '2025-04-01 00:00:00' ]
  ii  = [( i, i = 1, size(str) )]

  call sort_character( str, ii )

  do i = 1, size(str)
    print *, 'Rank:', i, ', Value: ', trim( str(ii(i)) )
  end do

end program

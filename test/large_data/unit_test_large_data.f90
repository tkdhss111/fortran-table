program unit_test_table_mo

  use table_mo
  use dt_mo, only: tm_ty

  implicit none

  type(table_ty) :: table, table1, table2

  type(tm_ty) :: tm

  print *, '=========================================='
  print *, 'Test: Large Data Set Processing' 
  print *, '=========================================='

  call table1%read_csv ( file = 'test_table_nrows10k.csv' )
  call table2%read_csv ( file = 'test_table_nrows165k.csv' )

  call table1%print
  call table2%print

  table1%key = 'date'
  table2%key = 'date'

  print *, '------------------------------------------'
  print *, 'Test: Inner Join (impure function) ... fast (-O3: <2sec)' 
  print *, '------------------------------------------'

  call tm%tic ()

  table = inner_join ( table1, table2 ) 

  call table%print

  call tm%toc ()

  call table%write_csv ( file = "inner_join_large_data.csv" )

!  print *, '------------------------------------------'
!  print *, 'Test: Inner Join (pure function) ... slow (-O3: 29sec)' 
!  print *, '------------------------------------------'
!
!  call tm%tic ()
!
!  table = inner_join_pure ( table1, table2 ) 
!
!  call table%print
!
!  call tm%toc ()

  print *, '------------------------------------------'
  print *, 'Test: Left Join (impure function) ... fast (-O3: <2sec)' 
  print *, '------------------------------------------'

  call tm%tic ()

  table = left_join ( table1, table2 ) 

  call table%print

  call tm%toc ()

  call table%write_csv ( file = "left_join_large_data.csv" )

!  print *, '------------------------------------------'
!  print *, 'Test: Left Join (pure function) ... slow (-O3: 28sec)' 
!  print *, '------------------------------------------'
!
!  call tm%tic ()
!
!  table = left_join_pure ( table1, table2 ) 
!
!  call table%print
!
!  call tm%toc ()

  print *, '------------------------------------------'
  print *, 'Test: Right Join (impure function) ... fast (-O3: <2sec)' 
  print *, '------------------------------------------'

  call tm%tic ()

  table = right_join ( table1, table2 ) 

  call table%print

  call tm%toc ()

  call table%write_csv ( file = "right_join_large_data.csv" )

!  print *, '------------------------------------------'
!  print *, 'Test: Right Join (pure function) ... slow (-O3: 44sec)' 
!  print *, '------------------------------------------'
!
!  call tm%tic ()
!
!  table = right_join_pure ( table1, table2 ) 
!
!  call table%print
!
!  call tm%toc ()

end program

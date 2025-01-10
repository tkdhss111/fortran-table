set ( EXE unit_test_large_data )
set ( SRCS ./unit_test_large_data.f90 ../../src/table_mo.f90 ./dt_mo.f90 )

set ( CMAKE_Fortran_FLAGS
  "-ffree-line-length-none -fcoarray=single")
set ( CMAKE_Fortran_FLAGS_DEBUG
  "-Ddebug -O3 -fbounds-check -fbacktrace -Wall -Wno-uninitialized -Wno-maybe-uninitialized ${CMAKE_Fortran_FLAGS}" )

add_executable ( ${EXE} ${SRCS} )
add_test ( NAME ${EXE} COMMAND ${EXE} WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} )

# fortran-table

Fortran Pure Functions for Table Manupulation

- SQL-like table manupulation
- All data are stored as character
- No database API is used

## Initialization

1. Minimum Initilization

```
call table%init ( nrows = 3, ncols = 3 )
```

2. Full Initialization

```
call table%init (                          &
    nrows    = 3,                          &
    ncols    = 3,                          &
    rownames = [ "row1", "row2", "row3" ], &
    colnames = [ "col1", "col2", "col3" ], &
    cell     = mydata,                     &
    key      = 'key',                      &
    name     = "mytable",                  &
    file     = "mytable.csv" )
```

## Select

```
table1 = select ( table, cols ) 
```
or
```
table1 = table%select( cols ) 
```

**cols** shall be column names / indeces / logical vector 

```
table1 = select ( table, [ "col3", "col2" ] )
```

```
table1 = select ( table, [ 3, 2 ] ) 
```

```
table1 = select ( table, [ .false., .true., .false. ] ) 
```

## Filter

```
table1 = filter ( table, rows ) 
```
or
```
table1 = table%filter( rows ) 
```

**rows** shall be row names / indeces / logical vector 

```
table1 = filter ( table, [ "row3", "row1" ] )
```

```
table1 = filter ( table, [ 3, 2 ] )
```

```
table1 = filter ( table, [ .true., .false., .true. ] )
```

## Join

key in each table is used for matching

### Inner Join

```
table3 = inner_join ( table1, table2 )
```
or
```
table3 = table1%inner_join ( table2 )
```

### Left Join

```
table3 = left_join ( table1, table2 )
```
or
```
table3 = table1%left_join ( table2 )
```
or
```
table3 = table1 * table2
```

### Right Join

```
table3 = right_join ( table1, table2 )
```
or
```
table3 = table1%right_join ( table2 )
```

## Record Insertion

### Insert or Replace

In the following example, 
if a record with the same key exits in table1, then it is replaced by the corresponding record in table2.
If there is no matching key in table1, then the record of table2 is appended to table1.
Records are sorted increasingly in accordance with the matching key order.

```
table3 = insert_or_replace ( table1, table2 )
```
or
```
table3 = table1%insert_or_replace ( table2 )
```
or
```
table3 = table1 + table2
```

### Append

Simple appending with no sorting of the two tables with the same columns

In the following example, table2 is appended to the end of table1.

```
table3 = append ( table1, table2 )
```
or
```
table3 = table1%append ( table2 )
```

## CSV File I/O

Write CSV file:

```
call table%write_csv ( file = "mytable.csv" )
```

Read CSV file:

```
call table%read_csv  ( file = "mytable.csv" )
```

## Column Converter

### Table column (character) -> {character, logical, integer, real} vector

```
values = table%to_{character, logical, integer, real} ( col )
```

**col** shall be integer or character

```
logical, allocatable :: logical_values(:)

logical_values = table%to_logical ( col = 2 )

```
or
```
logical_values = table%to_logical ( col = "col2" )
```

### {character, logical, integer, real} vector -> Table column (character)

```
call table%from_{character, logical, integer, real} ( vals, col )
```

**col** shall be integer or character

```
call table%from_logical ( logical_values, col = 2 )
```
or
```
call table%from_logical ( logical_values, col = "col2" )
```

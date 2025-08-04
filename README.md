# fortran-table

Fortran Functions for Table Manipulation

- SQL-like table manipulation
- All data are stored as character
- No database API is used
- Can handle large size table in joins (-O2, -O3)

## Initialization

1. Minimum Initialization

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
    key      = 'datetime',                 &
    name     = "mytable",                  &
    file     = "mytable.csv" )
```

## Select

```
table1 = select( table, cols )
```
or
```
table1 = table%select( cols )
```

**cols** shall be column names / indices / logical vector

```
table1 = select( table, [ "col3", "col2" ] )
```

```
table1 = select( table, [ 3, 2 ] )
```

```
table1 = select( table, [ .false., .true., .false. ] )
```

## Filter

```
table1 = filter( table, rows )
```
or
```
table1 = table%filter( rows )
```

**rows** shall be row names / indices / logical vector 

```
table1 = filter( table, [ "row3", "row1" ] )
```

```
table1 = filter( table, [ 3, 2 ] )
```

```
table1 = filter( table, [ .true., .false., .true. ] )
```

## Delete

```
table1 = delete( table, rows )
```
or
```
table1 = table%delete( rows )
```

**rows** shall be row names / indices / logical vector 

```
table1 = delete( table, [ "row3", "row1" ] )
```

```
table1 = delete( table, [ 3, 2 ] )
```

```
table1 = delete( table, [ .true., .false., .true. ] )
```

## Insert Column

```
table1 = table%insert_col( x, before, colname ) 
```

**x** shall be character / logical / integer / real vector for new column

**before**(optional) shall be character / integer for column position to insert before

**colname**(optional) shall be character for new column name

## Joins

key in each table is used for matching

### Inner Join

```
table3 = inner_join( table1, table2 )
```
or
```
table3 = table1%inner_join( table2 )
```

### Left Join

```
table3 = left_join( table1, table2 )
```
or
```
table3 = table1%left_join( table2 )
```
or
```
table3 = table1 * table2
```

### Right Join

```
table3 = right_join( table1, table2 )
```
or
```
table3 = table1%right_join( table2 )
```

## Record Insertion

### Insert or Replace

In the following example, 
if a record with the same key exits in table1, then it is replaced by the corresponding record in table2. The first column is used for the key if not specified.
If there is no matching key in table1, then the record of table2 is appended to table1.

```
table3 = insert_or_replace( table1, table2 )
```
or
```
table3 = table1%insert_or_replace( table2 )
```
or
```
table3 = table1 + table2
```

### Append

Simple appending with no sorting of the two tables with the same columns

In the following example, table2 is appended to the end of table1.

```
table3 = append( table1, table2 )
```
or
```
table3 = table1%append( table2 )
```

## CSV File I/O

Write CSV file:

```
call table%write_csv( file = "mytable.csv" )
```

Read CSV file:

```
call table%read_csv( file = "mytable.csv" )
```

Write Parquet file:

```
call table%write_parquet( file = "mytable.parquet" )
```

## Column Converter

### Table column (character) -> {character, logical, integer, real} vector

```
values = table%to_{character, logical, integer, real}( col )
```

**col** shall be integer or character

```
logical, allocatable :: logical_values(:)

logical_values = table%to_logical( col = 2 )

```
or
```
logical_values = table%to_logical( col = "col2" )
```

### {character, logical, integer, real} vector -> Table column (character)

```
call table%from_{character, logical, integer, real}( vals, col )
```

**col** shall be integer or character

```
call table%from_logical( logical_values, col = 2 )
```
or
```
call table%from_logical( logical_values, col = "col2" )
```

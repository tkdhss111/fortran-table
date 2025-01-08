rm( list = ls() )
library("duckdb")
library("dplyr")

con <- dbConnect(duckdb())
dbSendQuery(con, "DROP TABLE IF EXISTS table1")
dbSendQuery(con, "DROP TABLE IF EXISTS table2")

table1 <- data.frame( key = c("key1",   "key2",  "key3"),
                     t1c2 = c("t1.12", "t1.22", "t1.32"),
                     t1c3 = c("t1.13", "t1.23", "t1.33"))

table2 <- data.frame(t1c1 = c("t2.11", "t2.21", "t2.31"),
                      key = c("key3",   "key2",  "key2"),
                     t1c3 = c("t2.13", "t2.23", "t2.33"))

dbWriteTable(con, "table1", table1)
dbWriteTable(con, "table2", table2)

# Inner Join
res <- dbSendQuery(con, "SELECT * FROM table1 JOIN table2 ON table1.key == table2.key ORDER BY table2.key")
(table <- dbFetch(res))

#   key  t1c2  t1c3  t1c1  key  t1c3
#1 key2 t1.22 t1.23 t2.21 key2 t2.23
#2 key2 t1.22 t1.23 t2.31 key2 t2.33
#3 key3 t1.32 t1.33 t2.11 key3 t2.13

# Left Join
res <- dbSendQuery(con, "SELECT * FROM table1 LEFT JOIN table2 ON table1.key == table2.key ORDER BY table2.key")
(table <- dbFetch(res))

#   key  t1c2  t1c3  t1c1  key  t1c3
#1 key2 t1.22 t1.23 t2.31 key2 t2.33
#2 key2 t1.22 t1.23 t2.21 key2 t2.23
#3 key3 t1.32 t1.33 t2.11 key3 t2.13
#4 key1 t1.12 t1.13  <NA> <NA>  <NA>

# Right Join
res <- dbSendQuery(con, "SELECT * FROM table1 RIGHT JOIN table2 ON table1.key == table2.key ORDER BY table2.key")
(table <- dbFetch(res))

#   key  t1c2  t1c3  t1c1  key  t1c3
#1 key2 t1.22 t1.23 t2.21 key2 t2.23
#2 key2 t1.22 t1.23 t2.31 key2 t2.33
#3 key3 t1.32 t1.33 t2.11 key3 t2.13

#
# Large Data
#
dbSendQuery(con, "DROP TABLE IF EXISTS table3")
dbSendQuery(con, "DROP TABLE IF EXISTS table4")
dbSendQuery(con, "CREATE TABLE table3 AS SELECT * FROM read_csv_auto('test_table_nrows10k.csv')")  # Calendar (date)
dbSendQuery(con, "CREATE TABLE table4 AS SELECT * FROM read_csv_auto('test_table_nrows165k.csv')") # Load (datetime, date) Missing 2015-03-05 -- 2016-03-31
head(table3 <- dbReadTable(con, "table3"))
head(table4 <- dbReadTable(con, "table4"))

# Inner Join
res <- dbSendQuery(con, "SELECT * FROM table3 JOIN table4 ON table3.date == table4.date WHERE table4.datetime > '2016-03-31 20:00:00' ORDER BY table4.date")
head(table <- dbFetch(res))

#        date dow name event    md gr year month day wk su mo tu we th fr sa wd ho bw af sp ab            datetime       date    mw
#1 2016-04-01  金 平日  <NA> 04-01  3 2016     4   1 14  0  0  0  0  0  1  0  1  0  0  0  0  0 2016-04-01 00:00:00 2016-04-01 25550
#2 2016-04-01  金 平日  <NA> 04-01  3 2016     4   1 14  0  0  0  0  0  1  0  1  0  0  0  0  0 2016-04-01 01:00:00 2016-04-01 24330
#3 2016-04-01  金 平日  <NA> 04-01  3 2016     4   1 14  0  0  0  0  0  1  0  1  0  0  0  0  0 2016-04-01 02:00:00 2016-04-01 23930
#4 2016-04-01  金 平日  <NA> 04-01  3 2016     4   1 14  0  0  0  0  0  1  0  1  0  0  0  0  0 2016-04-01 03:00:00 2016-04-01 23750
#5 2016-04-01  金 平日  <NA> 04-01  3 2016     4   1 14  0  0  0  0  0  1  0  1  0  0  0  0  0 2016-04-01 04:00:00 2016-04-01 23900
#6 2016-04-01  金 平日  <NA> 04-01  3 2016     4   1 14  0  0  0  0  0  1  0  1  0  0  0  0  0 2016-04-01 05:00:00 2016-04-01 24670

# Left Join
res <- dbSendQuery(con, "SELECT * FROM table3 LEFT JOIN table4 ON table3.date == table4.date WHERE table3.date >= DATE '2016-03-30' ORDER BY table3.date")
head(table <- dbFetch(res))

#        date dow name event    md gr year month day wk su mo tu we th fr sa wd ho bw af sp ab            datetime       date    mw
#1 2016-03-30  水 平日  <NA> 03-30  3 2016     3  30 14  0  0  0  1  0  0  0  1  0  0  0  0  0                <NA>       <NA>    NA
#2 2016-03-31  木 平日  <NA> 03-31  3 2016     3  31 14  0  0  0  0  1  0  0  1  0  0  0  0  0                <NA>       <NA>    NA
#3 2016-04-01  金 平日  <NA> 04-01  3 2016     4   1 14  0  0  0  0  0  1  0  1  0  0  0  0  0 2016-04-01 00:00:00 2016-04-01 25550
#4 2016-04-01  金 平日  <NA> 04-01  3 2016     4   1 14  0  0  0  0  0  1  0  1  0  0  0  0  0 2016-04-01 01:00:00 2016-04-01 24330
#5 2016-04-01  金 平日  <NA> 04-01  3 2016     4   1 14  0  0  0  0  0  1  0  1  0  0  0  0  0 2016-04-01 02:00:00 2016-04-01 23930
#6 2016-04-01  金 平日  <NA> 04-01  3 2016     4   1 14  0  0  0  0  0  1  0  1  0  0  0  0  0 2016-04-01 03:00:00 2016-04-01 23750

dbClearResult(res)
dbDisconnect(con, shutdown = T)

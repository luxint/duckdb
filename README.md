# duckdb
newLISP bindings for [DuckDB](https://duckdb.org/), an embeddable SQL OLAP database management system.
In practice duckdb is a lot like SQLITE and you can use most of the same SQL syntax and functions but instead 
of row based duckdb is columnar based, which means queries are very fast. It also includes so some very easy and fast csv
import functions.

As Duckdb is still in early stages (pre 1.0) these bindings may break with older/newer versions, these bindings are using a dynamic library which i compiled on 23 augustus 2020, anything earlier or later may not work, so please be warned!

In these bindings've added a pivot command, which wil give you pivot functionality like in e and  but this one will support millions of lines/records!

So what do you need to install to get this working?
  1. Install [newLISP](http://www.newlisp.org/)
  2. Compile a version of duckdb from [github](https://github.com/cwida/duckdb)
  3. Create a duckdb directory in your home directory
  4. In this directory put the generated duckdb.so|dll|dynlib and duck.lsp and pivot.lsp 
  5. And then a typical workflow could look like this:
  





See documentation: https://htmlpreview.github.io/?https://github.com/luxint/duckdb/master/blob/doc/index.html


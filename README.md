# duckdb
newLISP bindings for [DuckDB](https://duckdb.org/), an embeddable SQL OLAP database management system.
In practice duckdb is a lot like SQLITE and you can use most of the same SQL syntax and functions but instead 
of row based duckdb is columnar based, which means queries are very fast. It also includes so some very easy and fast csv
import functions.

As Duckdb is still in early stages (pre 1.0) these bindings may break with older/newer versions, these bindings are using a dynamic library which i compiled on 23 augustus 2020, anything earlier or later may not work, so please be warned!

In these bindings've added a pivot command, which wil give you pivot functionality like in excel but this one will support millions of lines/records and sub-second responses!

So what do you need to install to get this working?
  1. Install [newLISP](http://www.newlisp.org/)
  2. Compile a version of duckdb from [github](https://github.com/cwida/duckdb)
  3. Create a duckdb directory in your home directory
  4. In this directory put the generated duckdb.so|dll|dynlib and duck.lsp and pivot.lsp 
  5. And then a typical workflow could look like this:

<pre><font color="#4E9A06"><b>ferry@hal</b></font>:<font color="#3465A4"><b>~/duckdb</b></font>$ newlisp pivot.lsp
newLISP v.10.7.5 64-bit on Linux IPv4/6 UTF-8 libffi, options: newlisp -h

&gt; (duck:open)
(true &quot;OK&quot;)
&gt; (duck:create-from-csv &quot;test/1500000 Sales Records.csv&quot; &quot;sales&quot; &quot;%m/%d/%Y&quot;)
((&quot;region&quot; &quot;VARCHAR&quot; &quot;YES&quot; 0 &quot;NULL&quot; 0) (&quot;country&quot; &quot;VARCHAR&quot; &quot;YES&quot; 0 &quot;NULL&quot; 0) (&quot;item_type&quot; 
  &quot;VARCHAR&quot; &quot;YES&quot; 0 &quot;NULL&quot; 0) 
 (&quot;sales_channel&quot; &quot;VARCHAR&quot; &quot;YES&quot; 0 &quot;NULL&quot; 0) 
 (&quot;order_priority&quot; &quot;VARCHAR&quot; &quot;YES&quot; 0 &quot;NULL&quot; 0) 
 (&quot;order_date&quot; &quot;DATE&quot; &quot;YES&quot; 0 &quot;NULL&quot; 0) 
 (&quot;order_id&quot; &quot;INTEGER&quot; &quot;YES&quot; 0 &quot;NULL&quot; 0) 
 (&quot;ship_date&quot; &quot;DATE&quot; &quot;YES&quot; 0 &quot;NULL&quot; 0) 
 (&quot;units_sold&quot; &quot;INTEGER&quot; &quot;YES&quot; 0 &quot;NULL&quot; 0) 
 (&quot;unit_price&quot; &quot;DOUBLE&quot; &quot;YES&quot; 0 &quot;NULL&quot; 0) 
 (&quot;unit_cost&quot; &quot;DOUBLE&quot; &quot;YES&quot; 0 &quot;NULL&quot; 0) 
 (&quot;total_revenue&quot; &quot;DOUBLE&quot; &quot;YES&quot; 0 &quot;NULL&quot; 0) 
 (&quot;total_cost&quot; &quot;DOUBLE&quot; &quot;YES&quot; 0 &quot;NULL&quot; 0) 
 (&quot;total_profit&quot; &quot;DOUBLE&quot; &quot;YES&quot; 0 &quot;NULL&quot; 0))
&gt; (duck &quot;select count() from sales;&quot;)
((1500000))
</pre>
<pre>&gt; (pivot &quot;sales&quot; &quot;region&quot; &quot;extract(&apos;year&apos; from order_date) as years&quot; &quot;units_sold&quot;)
YEARS                             |2010      |2011      |2012      |2013      |2014      |2015      |2016      |2017      |
REGION                            |units_sold|units_sold|units_sold|units_sold|units_sold|units_sold|units_sold|units_sold|
----------------------------------|----------|----------|----------|----------|----------|----------|----------|----------|
Asia                              | 143108867| 144151363| 144056421| 144659827| 144526441| 143276852| 146157279|  82659712|
Australia and Oceania             |  79886139|  80513521|  80437191|  80222621|  80035617|  79764882|  80155091|  46454749|
Central America and the Caribbean | 106391881| 105894508| 106125653| 106848982| 107398757| 107169289| 108682856|  61384143|
Europe                            | 257007883| 257768599| 256568200| 256433824| 256406459| 255806024| 257859106| 148537245|
Middle East and North Africa      | 122075239| 123341972| 123378290| 122680952| 124153381| 122746716| 123226768|  70114453|
North America                     |  21133324|  21719367|  21599898|  21194547|  21768204|  21078386|  21241773|  12632536|
Sub-Saharan Africa                | 255587287| 259352146| 258053384| 257379228| 255098022| 257288632| 258149122| 147613392|
</pre>

<pre>&gt; (pivot &quot;sales&quot; &quot;region, sales_channel&quot; &quot;extract(&apos;year&apos; from order_date) as years&quot; &quot;units_sold&quot;)
                                  |YEARS     |2010      |2011      |2012      |2013      |2014      |2015      |2016      |2017      |
REGION                            |SALES_CHA~|units_sold|units_sold|units_sold|units_sold|units_sold|units_sold|units_sold|units_sold|
----------------------------------|--------  |----------|----------|----------|----------|----------|----------|----------|----------|
Asia                              |Offline   |  71849499|  72271072|  71982412|  72224430|  72353319|  71311186|  72848762|  41148383|
Asia                              |Online    |  71259368|  71880291|  72074009|  72435397|  72173122|  71965666|  73308517|  41511329|
Australia and Oceania             |Offline   |  40074199|  40444915|  39871606|  40094937|  40136724|  39832833|  40031137|  23131826|
Australia and Oceania             |Online    |  39811940|  40068606|  40565585|  40127684|  39898893|  39932049|  40123954|  23322923|
Central America and the Caribbean |Offline   |  53060656|  52986214|  53647237|  53568939|  53914516|  53756254|  54563176|  30885820|
Central America and the Caribbean |Online    |  53331225|  52908294|  52478416|  53280043|  53484241|  53413035|  54119680|  30498323|
Europe                            |Offline   | 128425973| 128546389| 127830278| 127462710| 128176022| 127713682| 129213893|  73974230|
Europe                            |Online    | 128581910| 129222210| 128737922| 128971114| 128230437| 128092342| 128645213|  74563015|
Middle East and North Africa      |Offline   |  61054441|  61962155|  61971027|  61373209|  62187601|  61780464|  62060783|  35025158|
Middle East and North Africa      |Online    |  61020798|  61379817|  61407263|  61307743|  61965780|  60966252|  61165985|  35089295|
North America                     |Offline   |  10608603|  10940663|  11138559|  10486526|  10919345|  10632216|  10546544|   6290339|
North America                     |Online    |  10524721|  10778704|  10461339|  10708021|  10848859|  10446170|  10695229|   6342197|
Sub-Saharan Africa                |Offline   | 127394459| 129287908| 129838757| 128773796| 127457601| 128866360| 129136252|  73606900|
Sub-Saharan Africa                |Online    | 128192828| 130064238| 128214627| 128605432| 127640421| 128422272| 129012870|  74006492|
</pre>




See documentation: https://htmlpreview.github.io/?https://github.com/luxint/duckdb/master/blob/doc/index.html


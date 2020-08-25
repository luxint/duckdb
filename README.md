# duckdb
newLISP bindings for [DuckDB](https://duckdb.org/), an embeddable SQL OLAP database management system.
In practice duckdb is a lot like SQLITE and you can use most of the same SQL syntax and functions but instead 
of row based duckdb is columnar based, which means queries are very fast. It also includes so some very easy and fast csv
import functions.

As Duckdb is still in early stages (pre 1.0) these bindings may break with older/newer versions, these bindings are using a dynamic library which was compiled on 23 augustus 2020, anything earlier or later may not work, so please be warned!

In these bindings i've added a pivot command, which wil give you pivot functionality like in excel but this one will support millions of lines/records and sub-second responses!

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

<pre>&gt; (pivot &quot;(select * from sales where country like &apos;N%&apos;)&quot; &quot;region, country, sales_channel&quot; &quot;extract(&apos;year&apos; from order_date) as years&quot; &quot;units_sold&quot;)
                                  |            |YEARS     |2010      |2011      |2012      |2013      |2014      |2015      |2016      |2017      |
REGION                            |COUNTRY     |SALES_CHA~|units_sold|units_sold|units_sold|units_sold|units_sold|units_sold|units_sold|units_sold|
----------------------------------|------------|--------  |----------|----------|----------|----------|----------|----------|----------|----------|
Asia                              |Nepal       |Offline   |   2636874|   2545253|   2566029|   2686264|   2636642|   2443001|   2689804|   1624166|
Asia                              |Nepal       |Online    |   2616712|   2543229|   2780224|   2819988|   2748969|   2615675|   2796795|   1560465|
Asia                              |North Korea |Offline   |   2559530|   2712558|   2666307|   2560877|   2839031|   2647753|   2598481|   1481824|
Asia                              |North Korea |Online    |   2713440|   2564453|   2702850|   2648516|   2743694|   2689818|   2570638|   1549521|
Australia and Oceania             |Nauru       |Offline   |   2493514|   2693936|   2704472|   2695096|   2579598|   2715988|   2643822|   1416968|
Australia and Oceania             |Nauru       |Online    |   2763751|   2351701|   2624333|   2826663|   2503948|   2585632|   2673008|   1540149|
Australia and Oceania             |New Zealand |Offline   |   2754750|   2671200|   2946205|   2570856|   2924049|   2648371|   2664845|   1617100|
Australia and Oceania             |New Zealand |Online    |   2560266|   2768616|   2813812|   2836630|   2596871|   2821694|   2672283|   1708252|
Central America and the Caribbean |Nicaragua   |Offline   |   2518914|   2624712|   2531085|   2723418|   2750794|   2577176|   2569984|   1564827|
Central America and the Caribbean |Nicaragua   |Online    |   2726017|   2650735|   2843632|   2626527|   2714827|   2830600|   2699966|   1407199|
Europe                            |Netherlands |Offline   |   2749106|   2797045|   2539947|   2605732|   2639907|   2601971|   2765823|   1574502|
Europe                            |Netherlands |Online    |   2856465|   2698682|   2773469|   2338133|   2929124|   2558309|   2516159|   1569682|
Europe                            |Norway      |Offline   |   2719708|   2802129|   2774850|   2657337|   2694623|   2734524|   2875907|   1529589|
Europe                            |Norway      |Online    |   2535312|   2559034|   2696699|   2777857|   2568460|   2601033|   2767511|   1529513|
Sub-Saharan Africa                |Namibia     |Offline   |   2577423|   2634613|   2678963|   2678889|   2555988|   2579300|   2627597|   1556092|
Sub-Saharan Africa                |Namibia     |Online    |   2722169|   2771930|   2664748|   2825909|   2689713|   2849257|   2823632|   1578482|
Sub-Saharan Africa                |Niger       |Offline   |   2541614|   2516827|   2708595|   2661975|   2794909|   2754812|   2901542|   1569193|
Sub-Saharan Africa                |Niger       |Online    |   2704701|   2661776|   2578637|   2778107|   2478880|   2625911|   2706882|   1434739|
Sub-Saharan Africa                |Nigeria     |Offline   |   2467203|   2703702|   2656639|   2863712|   2590780|   2788497|   2634856|   1501607|
Sub-Saharan Africa                |Nigeria     |Online    |   2677640|   2678009|   2760264|   2599663|   2520871|   2501459|   2663447|   1694314|
</pre>

<pre>&gt; (pivot &quot;(select * from sales where country like &apos;N%&apos;)&quot; &quot;region, country&quot; &quot;sales_channel&quot; &quot;order_date as orders, total_revenue as avg_revenue&quot; &quot;count, avg&quot;)
                                  |SALES_CHANN~|Offline   |Offline    |Online    |Online     |
REGION                            |COUNTRY     |orders    |avg_revenue|orders    |avg_revenue|
----------------------------------|------------|-------   |-----------|------    |-----------|
Asia                              |Nepal       |      4033|    1312809|      4109|    1318191|
Asia                              |North Korea |      4010|    1331013|      4049|    1325554|
Australia and Oceania             |Nauru       |      3974|    1320193|      4003|    1324690|
Australia and Oceania             |New Zealand |      4138|    1335157|      4138|    1317391|
Central America and the Caribbean |Nicaragua   |      4016|    1317127|      4035|    1350123|
Europe                            |Netherlands |      4106|    1317491|      4052|    1307760|
Europe                            |Norway      |      4156|    1351320|      4039|    1318156|
Sub-Saharan Africa                |Namibia     |      3971|    1352107|      4172|    1320427|
Sub-Saharan Africa                |Niger       |      4102|    1329495|      4001|    1338779|
Sub-Saharan Africa                |Nigeria     |      4009|    1339594|      4024|    1351274|
</pre>

See documentation: https://htmlpreview.github.io/?https://github.com/luxint/duckdb/master/blob/doc/index.html


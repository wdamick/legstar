# Introduction #

In [COBOLBindingConversionMetrics](COBOLBindingConversionMetrics.md) we presented performance metrics for elementary data item conversions.

Here we perform the same type of measurement but applied to complex structures transformations from mainframe format to java and vice-versa.

# Use case #

We use the [DPLARCHT](http://code.google.com/p/legstar-cob2xsd/source/browse/trunk/src/test/resources/cobol/DPLARCHT) COBOL program commarea as it presents an interesting mix of COBOL REDEFINES and ARRAYS DEPENDING ON.

REDEFINES cases add complexity as it is necessary to chose between alternatives at runtime.

ARRAYS DEPENDING ON add complexity because it is necessary to dynamically track the array size.

The DEPENDING ON array is also a convenient way to play on the COBOL data length and check how the number of elementary data items influences the overall performance.

# Measurement procedure #

The JUnit test case available [here](http://code.google.com/p/legstar/source/browse/trunk/legstar-core/legstar-coxbgen/src/test/java/com/legstar/test/coxb/perf/DplarchtMeteringTest.java) takes a parameter that gives the variable array size (0 to 500).

The testHostToJava method turns mainframe data to a java object and testJavaToHost method does the reverse.

There is no attempt to reuse the Transformers. Each test execution re-creates the Transformers which is not optimal as creating Transformers will also recreate all bindings (one for each data item).

We ran the tests for 5 different number of items for the variable size array: 100, 200, 300, 400 and 500. This is the relationship between this number of array items, the number of elementary data items processed and the mainframe data size:

| **Array items** | **Data items** | **Payload (bytes)** |
|:----------------|:---------------|:--------------------|
| 100             | 306            | 6425                |
| 200             | 606            | 12825               |
| 300             | 906            | 19225               |
| 400             | 1206           | 25625               |
| 500             | 1506           | 32025               |

We used [Apache JMeter 2.4](http://jakarta.apache.org/jmeter/) to load test each one of these JUnit methods. Each test was run 100000 times with 1000 warmup iterations.

The JMeter configuration files can be found [here](http://code.google.com/p/legstar/source/browse/trunk/legstar-core/legstar-coxbgen/src/test/#test/resources/jmeter/transform).

Target machine and quantities measured are identical to [COBOLBindingConversionMetrics](COBOLBindingConversionMetrics.md).

# Results #

## Release 1.4.1 ##

![http://legstar.googlecode.com/svn/wiki/dplarcht-transformations-metrics-1.4.1.png](http://legstar.googlecode.com/svn/wiki/dplarcht-transformations-metrics-1.4.1.png)

# Conclusion #

In the most complex case with 1506 data items and 32025 bytes of host data, it was still possible to reach about 500 TPS. This is in conditions that are not particularly favorable (Transformers are not reused, JMeter in GUI mode and JUnit assertions performed on each transformation).

The java to host transformations are slightly faster (The difference was greater with previous LegStar releases).

Also of interest is the fact that the throughput does not appear to decrease in a linear fashion with the number of items in the variable size array.

DPLARCHT is a 5 levels deep structure which is probably average. The number of levels has an impact on performances because each level is a complex binding with a significant overhead. Shallow structures perform better than deep structures but this would probably deserve further study.
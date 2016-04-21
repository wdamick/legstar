# Introduction #

There are several performance aspects of interest in LegStar. Here we examine the behavior of the COBOL conversions at the data type (or individual field) level.

At the finest level, the LegStar COBOL binding runtime performs elemental conversions from COBOL data types to Java types. So we first focus on these data types conversions as they will affect all higher level features.

The metrics are focused on CPU as this is likely to be the most important resource from a performance perspective.

# Measurement procedure #

We bundled JUnit test cases for each of the functions we wanted to measure:

| **Test name**          | **Description** |
|:-----------------------|:----------------|
| AlphanumericToString   | PIC X(43) to Java String using IBM01147 EBCDIC character set |
| PackedDecimalToBigDecimal | PIC S9(15)V99 COMP-3 to Java BigDecimal |
| ZonedDecimalToBigDecimal | PIC S9(15)V99 to Java BigDecimal |
| BinaryToBigDecimal     | PIC S9(8) COMP to Java BigDecimal |
| StringToAlphanumeric   | Java String to PIC X(43) using IBM01147 EBCDIC character set |
| BigDecimalToPackedDecimal | Java BigDecimal to PIC S9(15)V99 COMP-3 |
| BigDecimalToZonedDecimal | Java BigDecimal to PIC S9(15)V99 |
| BigDecimalToBinary     | Java BigDecimal to PIC S9(8) COMP |

You can find the JUnit source code [here](http://code.google.com/p/legstar/source/browse/trunk/legstar-core/legstar-coxbrt/src/test/java/com/legstar/coxb/convert/simple/MeteringTest.java).

We used [Apache JMeter version 2.4](http://jakarta.apache.org/jmeter/) to load test each one of these JUnit methods. Each test was run 1000000 times with 10000 warmup iterations.

The JMeter configuration files can be found [here](http://code.google.com/p/legstar/source/browse/#svn/trunk/legstar-core/legstar-coxbrt/src/test/resources/jmeter/convert/simple).

The target machine is an Intel centrino dual core 2.00 Ghz, with 2 Gbytes of RAM, running JRE 1.6.0\_16.

We measured the throughput which is the number of elementary conversions we could perform in a second.

The results should not be interpreted as absolute values. Typically we were running JMeter in GUI mode and performing JUnit assertion on each execution which tends to use CPU cycles and reduce throughput. The results are valid when you compare individual tests to each other though.

# Results #

## LegStar release 1.4.1 ##

| **Test name**          | **Conversions per second** |
|:-----------------------|:---------------------------|
| COBOL Alphanumeric To Java String	| 36778                      |
| COBOL Binary To Java BigDecimal	| 39505                      |
| COBOL ZonedDecimal To Java BigDecimal	| 36832                      |
| COBOL PackedDecimal To Java BigDecimal	| 37932                      |
| Java String To COBOL Alphanumeric	| 36167                      |
| Java BigDecimal To COBOL Binary	| 40126                      |
| Java BigDecimal To COBOL ZonedDecimal	| 35602                      |
| Java BigDecimal To COBOL PackedDecimal	| 38471                      |

# Conclusion #

Elementary conversions for basic types perform in the same range.

in release 1.4.1, conversions have been optimized to reduce the number of java objects allocated. Java objects were replaced by primitive types whenever possible. This has some impact on CPU but perhaps more importantly has reduced the memory footprint.
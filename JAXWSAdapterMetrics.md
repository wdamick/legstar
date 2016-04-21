# Introduction #

The objective here is to measure performances of LegStar Web Service adapters.

Web Service adapters are JAX-WS endpoints, they can be deployed to any JAX-WS capable server. Here we deploy to Sun's JAX-WS RI 2.1.3 (Metro) and Apache AXIS2 1.5.

# Use case #

We use the [LSFILEAE](http://code.google.com/p/legstar/source/browse/trunk/legstar-schemagen/src/test/cobol/LSFILEAE) COBOL/CICS program.

LSFILEAE has a very simple commarea so transformations do not significantly affect performance. (see COBOLBindingTransformationMetrics if you are interested in Transformation performances).

In the case of JAX-WS RI, the LegStar Adapter is deployed as a war file in Tomcat 6.0.

In the case of AXIS2, AXIS2 itself is deployed as a war file in Tomcat 6.0. The LegStar Adapter is deployed as a jar file to the AXIS2 servicejars folder.

# Measurement procedure #

Again we use a combination of [Apache JMeter](http://jakarta.apache.org/jmeter/)/JUnit as our test bench. The JUnit tests are available [here](http://code.google.com/p/legstar/source/browse/trunk/legstar-cixsgen-cases/src/test/java/com/legstar/test/cixs/). We used HttpClientLsfileae100Test and HttpClientLsfileae100AxisTest which implement a simple http client sending raw SOAP over HTTP. These tests takes a parameter which should map to one of your configured mainframe endpoints (usually described in legstar-invoker-config.xml in your Tomcat classpath). The JMeter test plans are also available for each transport, check the src/test/resources/jmeter sub folder.

We used the Mock transport (Available with LegStar for testing purposes) which simulates access to the mainframe. This factors out the transport related performances (see TransportMetrics if you are interested in these).

For each test we simulate 2 clients with no think time. We can get much higher but we wanted the measures to be compatible with the one from TransportMetrics.

# Results #

## Release 1.2.6 ##

| **WS Stack** | **Transport** | **Transaction rate (TPS)** | **Average response time (ms)** |
|:-------------|:--------------|:---------------------------|:-------------------------------|
| JAX-WS RI	   | Mock          | 124.2                      | 15                             |
| AXIS 2	      | Mock          | 123.5                      | 15                             |

# Conclusion #

There are no observable differences between a JAX-WS RI and an AXIS2 deployment.

In a future study, we will compare these results with Adapters deployed to other containers such as JBoss WS, JBoss ESB and MuleSource.
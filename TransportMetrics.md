# Introduction #

The objective here is to compare the performances of the various transports offered by LegStar for adapters.

The core LegStar project comes with Sockets, HTTP and WebSphere MQ transports for adapters.

# Use case #

We use the [LSFILEAE](http://code.google.com/p/legstar/source/browse/trunk/legstar-schemagen/src/test/cobol/LSFILEAE) COBOL/CICS program.

LSFILEAE is a very simple program that performs a single lookup into a VSAM file.

We don't use any transformations or Web Service wrappering. We send and receive raw mainframe data. This allows us to focus on the transport layer without consideration for transformations (which are covered in COBOLBindingTransformationMetrics).

# Measurement procedure #

Again we use a combination of [Apache JMeter](http://jakarta.apache.org/jmeter/)/JUnit as our test bench. The JUnit test cases are available in each transport. The test classes names end with the "MeteringTest" suffix. The JMeter test plans are also available for each transport, check the src/test/resources/jmeter sub folder.

The measurements performed here are against a development z/OS machine with extremely reduced CPU capabilities and relatively old versions of both z/OS (1.5) and CICS (TS 2.3 and TS 3.1). We recommend that you don't consider absolute values but rather relative performances. The objective being to compare the various transports, we consider the results still valid (see later for caution about this). We recommend to rerun the tests on your own machines if you need to get absolute values and then let us know your results.

We perform authentication to the mainframe on each new connection. There is a performance impact of doing so that we did not measure here.

With HTTP, we don't use SSL. HTTP protocol version is 1.0 with TS 2.3 and 1.1 with TS 3.1.

With WebSphere MQ, messages are not persisted and are non transactional.

For each test we simulate 2 clients with no think time. Our small z/OS machine is at 100% CPU most of the time so we can't get any higher. Each client sends 1000 requests.

# Results #

## Release 1.2.6 ##

| **Transport** | **Pooling** |**CICS** | **Transaction rate (TPS)** | **Average response time (ms)** |
|:--------------|:------------|:--------|:---------------------------|:-------------------------------|
| HTTP	         | No          |TS 2.3   | 19.5                       | 101                            |
| HTTP	         | Yes         |TS 2.3   | 19.4                       | 102                            |
| HTTP	         | No          |TS 3.1   | 12.7                       | 156                            |
| HTTP	         | Yes         |TS 3.1   | 14.9                       | 132                            |
| WebSphere MQ	 | No          |TS 2.3 (LegStar messaging)   | 4.3                        | 465                            |
| WebSphere MQ	 | Yes         |TS 2.3 (LegStar messaging)   | 22.3                       | 88                             |
| WebSphere MQ	 | No          |TS 3.1 (MQCIH messaging)   | 3.6                        | 554                            |
| WebSphere MQ	 | Yes         |TS 2.3 (MQCIH messaging)   | 11.2                       | 178                            |
| Socket	       | No          |TS 2.3   | 13.4                       | 149                            |
| Socket	       | Yes         |TS 2.3   | 35.1                       | 55                             |
| Socket	       | No          |TS 3.1   | 11.5                       | 173                            |
| Socket	       | Yes         |TS 3.1   | 36.2                       | 54                             |

# Conclusion #

When using HTTP and TS 2.3, pooling is useless. This is because TS 2.3 does not support HTTP 1.1 and connections are not kept alive.

There is a sharp degradation of performances between HTTP TS 2.3 and HTTP TS 3.1. This could be because IBM introduced a heavier web support in TS 3.1 but most probably because we are using an unpatched version of CICS TS 3.1. The fact that there is little difference between pooling, where HTTP connections are actually reused, and not pooling confirms that there is something abnormal with our HTTP TS 3.1 setting. So be cautious with these results.

The version of WebSphere MQ used is quite old (5.3). There is a high cost to opening/closing queues on each request. Pooling connections in these conditions is very effective.

With WebSphere MQ, we support 2 different types of messaging. LegStar messaging requires a LegStar MQ server transaction in CICS while MQCIH uses the standard IBM CICS MQ Bridge support. We have considered that the CICS version impact is insignificant which is why we did not run all combinations of these tests.

It is interesting to note that performances range from 36.2 to 3.6 TPS to handle the exact same workload.

Of course these measures are very limited, they don't cover security, transactions and persistence and, as mentioned, use a hardware/software combination that is questionable at best. The good thing though is that a set of tested JMeter/JUnit tests are available for you to perform your own testing on your own configurations. No doubts that results will be quite different.

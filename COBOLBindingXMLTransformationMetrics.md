# Introduction #

XML Transformers as opposed to Java Transformers, transform XML payloads to Host byte arrays and vice-versa.

They are much more heavyweight than java Transformers because they involve the JAXB infrastructure for XML.

This wiki applies to the case where you use standard [JAXB](http://jaxb.java.net) as provided in JDK 1.6 and up (JAXB RI for JDK 1.5).

We focus on XML to Host transformations although the results apply to Host to XML as well.

# Performance testing #

We use a set of JUnit test cases very similar to those described in [COBOLBindingTransformationMetrics](COBOLBindingTransformationMetrics.md). You should probably read that wiki page before this one.

The new JUnit test cases for XML Transformers are [here](http://code.google.com/p/legstar/source/browse/trunk/legstar-core/legstar-coxbgen/src/test/java/com/legstar/test/coxb/perf/DplarchtMeteringTest.java) and [here](http://code.google.com/p/legstar/source/browse/trunk/legstar-core/legstar-coxbgen/src/test/java/com/legstar/test/coxb/perf/DplarchtMeteringWithReuseTest.java), look for methods:

  * testHostToXml

  * testXmlToHost


Running DplarchtMeteringTest#testHostToXml with the largest 500 payload (the corresponding XML is about 100K), the throughput for Host to XML is about 8 TPS. As a comparison, Host to Java is about 540 TPS.

Using the Yourkit profiler, reveals the hotspot: 91% of the time is spent in javax.xml.bind.JAXBContext.newInstance(Class[.md](.md)) while instantiating the Transformers.

[JAXBContext](http://docs.oracle.com/javaee/6/api/javax/xml/bind/JAXBContext.html) is a heavyweight object and it is very costly to instantiate. It is thread safe so you could assume we can get away with making Transformer classes static but the Transformers also create JAXB Marshallers and Unmarshallers at construction time and these are not thread safe.

The solution proposed here makes the assumption that you are running the Transformers in an environment that provides a Thread pool. JEE and ESB environments typically do that.

The solution uses the java [ThreadLocal](http://docs.oracle.com/javase/6/docs/api/java/lang/ThreadLocal.html) class to keep a separate copy of a reusable Transformers class in each thread. If you look at the DplarchtMeteringWithReuseTest test case, you will notice this:

```
    private static final ThreadLocal < DfhcommareaXmlTransformers > _xmlTransformers = new ThreadLocal < DfhcommareaXmlTransformers >() {
        @Override
        protected DfhcommareaXmlTransformers initialValue() {
            try {
                return new DfhcommareaXmlTransformers();
            } catch (HostTransformException e) {
                return null;
            }
        }
    };
```

Then, when needed, instead of instantiating a new Transformers class, you do:

```
   DfhcommareaXmlTransformers transformers = _xmlTransformers.get();
```

That's it.

Back to load testing, with Transformers reuse, the throughput is now 220 TPS. Much better than 8!

We are still far from the 540 TPS we get with java. The profiler again reveals that 42% of the time is spent in javax.xml.bind.helpers.AbstractMarshallerImpl.marshal(Object, Writer). That's the JAXB XML formatting piece. Not a lot we can do about that.

# Conclusion #

Transformers for XML are very expensive to instantiate. There are dramatic gains in reusing these Transformers.

The simple ThreadLocal solution proposed here works in most cases but is not the only solution. A pool of Transformers can also be created (see [Apache commons pooling](http://commons.apache.org/pool/) for instance).




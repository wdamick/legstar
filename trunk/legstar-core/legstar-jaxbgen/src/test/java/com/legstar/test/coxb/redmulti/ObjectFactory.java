
package com.legstar.test.coxb.redmulti;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.coxb.redmulti package. 
 * <p>An ObjectFactory allows you to programatically 
 * construct new instances of the Java representation 
 * for XML content. The Java representation of XML 
 * content can consist of schema derived interfaces 
 * and classes representing the binding of schema 
 * type definitions, element declarations and model 
 * groups.  Factory methods for each of these are 
 * provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {

    private final static QName _Dfhcommarea_QNAME = new QName("http://legstar.com/test/coxb/redmulti", "Dfhcommarea");
    private final static QName _Filler20_QNAME = new QName("http://legstar.com/test/coxb/redmulti", "Filler20");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.coxb.redmulti
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link Filler20 }
     * 
     */
    public Filler20 createFiller20() {
        return new Filler20();
    }

    /**
     * Create an instance of {@link Filler35 }
     * 
     */
    public Filler35 createFiller35() {
        return new Filler35();
    }

    /**
     * Create an instance of {@link Dfhcommarea }
     * 
     */
    public Dfhcommarea createDfhcommarea() {
        return new Dfhcommarea();
    }

    /**
     * Create an instance of {@link Filler38 }
     * 
     */
    public Filler38 createFiller38() {
        return new Filler38();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Dfhcommarea }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/redmulti", name = "Dfhcommarea")
    public JAXBElement<Dfhcommarea> createDfhcommarea(Dfhcommarea value) {
        return new JAXBElement<Dfhcommarea>(_Dfhcommarea_QNAME, Dfhcommarea.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Filler20 }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/redmulti", name = "Filler20")
    public JAXBElement<Filler20> createFiller20(Filler20 value) {
        return new JAXBElement<Filler20>(_Filler20_QNAME, Filler20 .class, null, value);
    }

}

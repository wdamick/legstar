
package com.legstar.test.coxb.arrayssm;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.coxb.arrayssm package. 
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

    private final static QName _WsTs_QNAME = new QName("http://legstar.com/test/coxb/arrayssm", "WsTs");
    private final static QName _WsTc_QNAME = new QName("http://legstar.com/test/coxb/arrayssm", "WsTc");
    private final static QName _Dfhcommarea_QNAME = new QName("http://legstar.com/test/coxb/arrayssm", "Dfhcommarea");
    private final static QName _WsTc2_QNAME = new QName("http://legstar.com/test/coxb/arrayssm", "WsTc2");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.coxb.arrayssm
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link Dfhcommarea }
     * 
     */
    public Dfhcommarea createDfhcommarea() {
        return new Dfhcommarea();
    }

    /**
     * Create an instance of {@link TableComplex }
     * 
     */
    public TableComplex createTableComplex() {
        return new TableComplex();
    }

    /**
     * Create an instance of {@link WsTs }
     * 
     */
    public WsTs createWsTs() {
        return new WsTs();
    }

    /**
     * Create an instance of {@link WsTc }
     * 
     */
    public WsTc createWsTc() {
        return new WsTc();
    }

    /**
     * Create an instance of {@link WsTc2 }
     * 
     */
    public WsTc2 createWsTc2() {
        return new WsTc2();
    }

    /**
     * Create an instance of {@link TableComplex2 }
     * 
     */
    public TableComplex2 createTableComplex2() {
        return new TableComplex2();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link WsTs }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/arrayssm", name = "WsTs")
    public JAXBElement<WsTs> createWsTs(WsTs value) {
        return new JAXBElement<WsTs>(_WsTs_QNAME, WsTs.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link WsTc }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/arrayssm", name = "WsTc")
    public JAXBElement<WsTc> createWsTc(WsTc value) {
        return new JAXBElement<WsTc>(_WsTc_QNAME, WsTc.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Dfhcommarea }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/arrayssm", name = "Dfhcommarea")
    public JAXBElement<Dfhcommarea> createDfhcommarea(Dfhcommarea value) {
        return new JAXBElement<Dfhcommarea>(_Dfhcommarea_QNAME, Dfhcommarea.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link WsTc2 }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/arrayssm", name = "WsTc2")
    public JAXBElement<WsTc2> createWsTc2(WsTc2 value) {
        return new JAXBElement<WsTc2>(_WsTc2_QNAME, WsTc2 .class, null, value);
    }

}

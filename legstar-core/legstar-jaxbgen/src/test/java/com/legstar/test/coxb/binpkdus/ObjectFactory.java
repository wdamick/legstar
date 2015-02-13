
package com.legstar.test.coxb.binpkdus;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.coxb.binpkdus package. 
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

    private final static QName _Filler45_QNAME = new QName("http://legstar.com/test/coxb/binpkdus", "Filler45");
    private final static QName _WsExpectedIncomingData_QNAME = new QName("http://legstar.com/test/coxb/binpkdus", "WsExpectedIncomingData");
    private final static QName _Dfhcommarea_QNAME = new QName("http://legstar.com/test/coxb/binpkdus", "Dfhcommarea");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.coxb.binpkdus
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link WsUnsignedPackedDecimal }
     * 
     */
    public WsUnsignedPackedDecimal createWsUnsignedPackedDecimal() {
        return new WsUnsignedPackedDecimal();
    }

    /**
     * Create an instance of {@link LsUnsignedPackedDecimal }
     * 
     */
    public LsUnsignedPackedDecimal createLsUnsignedPackedDecimal() {
        return new LsUnsignedPackedDecimal();
    }

    /**
     * Create an instance of {@link WsCompat }
     * 
     */
    public WsCompat createWsCompat() {
        return new WsCompat();
    }

    /**
     * Create an instance of {@link LsExtend }
     * 
     */
    public LsExtend createLsExtend() {
        return new LsExtend();
    }

    /**
     * Create an instance of {@link WsExpectedIncomingData }
     * 
     */
    public WsExpectedIncomingData createWsExpectedIncomingData() {
        return new WsExpectedIncomingData();
    }

    /**
     * Create an instance of {@link WsExtend }
     * 
     */
    public WsExtend createWsExtend() {
        return new WsExtend();
    }

    /**
     * Create an instance of {@link Dfhcommarea }
     * 
     */
    public Dfhcommarea createDfhcommarea() {
        return new Dfhcommarea();
    }

    /**
     * Create an instance of {@link Filler45 }
     * 
     */
    public Filler45 createFiller45() {
        return new Filler45();
    }

    /**
     * Create an instance of {@link LsCompat }
     * 
     */
    public LsCompat createLsCompat() {
        return new LsCompat();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Filler45 }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/binpkdus", name = "Filler45")
    public JAXBElement<Filler45> createFiller45(Filler45 value) {
        return new JAXBElement<Filler45>(_Filler45_QNAME, Filler45 .class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link WsExpectedIncomingData }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/binpkdus", name = "WsExpectedIncomingData")
    public JAXBElement<WsExpectedIncomingData> createWsExpectedIncomingData(WsExpectedIncomingData value) {
        return new JAXBElement<WsExpectedIncomingData>(_WsExpectedIncomingData_QNAME, WsExpectedIncomingData.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Dfhcommarea }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/binpkdus", name = "Dfhcommarea")
    public JAXBElement<Dfhcommarea> createDfhcommarea(Dfhcommarea value) {
        return new JAXBElement<Dfhcommarea>(_Dfhcommarea_QNAME, Dfhcommarea.class, null, value);
    }

}

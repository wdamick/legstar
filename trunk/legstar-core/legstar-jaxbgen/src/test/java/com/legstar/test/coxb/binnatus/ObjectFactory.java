
package com.legstar.test.coxb.binnatus;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.coxb.binnatus package. 
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

    private final static QName _WsExpectedIncomingData_QNAME = new QName("http://legstar.com/test/coxb/binnatus", "WsExpectedIncomingData");
    private final static QName _Filler55_QNAME = new QName("http://legstar.com/test/coxb/binnatus", "Filler55");
    private final static QName _Dfhcommarea_QNAME = new QName("http://legstar.com/test/coxb/binnatus", "Dfhcommarea");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.coxb.binnatus
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link WsExpectedIncomingData }
     * 
     */
    public WsExpectedIncomingData createWsExpectedIncomingData() {
        return new WsExpectedIncomingData();
    }

    /**
     * Create an instance of {@link WsFullwords }
     * 
     */
    public WsFullwords createWsFullwords() {
        return new WsFullwords();
    }

    /**
     * Create an instance of {@link LsDoublewords }
     * 
     */
    public LsDoublewords createLsDoublewords() {
        return new LsDoublewords();
    }

    /**
     * Create an instance of {@link Dfhcommarea }
     * 
     */
    public Dfhcommarea createDfhcommarea() {
        return new Dfhcommarea();
    }

    /**
     * Create an instance of {@link WsP9X18HighB }
     * 
     */
    public WsP9X18HighB createWsP9X18HighB() {
        return new WsP9X18HighB();
    }

    /**
     * Create an instance of {@link LsHalfwords }
     * 
     */
    public LsHalfwords createLsHalfwords() {
        return new LsHalfwords();
    }

    /**
     * Create an instance of {@link WsUnsignedNative }
     * 
     */
    public WsUnsignedNative createWsUnsignedNative() {
        return new WsUnsignedNative();
    }

    /**
     * Create an instance of {@link WsHalfwords }
     * 
     */
    public WsHalfwords createWsHalfwords() {
        return new WsHalfwords();
    }

    /**
     * Create an instance of {@link Filler55 }
     * 
     */
    public Filler55 createFiller55() {
        return new Filler55();
    }

    /**
     * Create an instance of {@link LsFullwords }
     * 
     */
    public LsFullwords createLsFullwords() {
        return new LsFullwords();
    }

    /**
     * Create an instance of {@link WsP9X18MaxB }
     * 
     */
    public WsP9X18MaxB createWsP9X18MaxB() {
        return new WsP9X18MaxB();
    }

    /**
     * Create an instance of {@link LsUnsignedNative }
     * 
     */
    public LsUnsignedNative createLsUnsignedNative() {
        return new LsUnsignedNative();
    }

    /**
     * Create an instance of {@link WsDoublewords }
     * 
     */
    public WsDoublewords createWsDoublewords() {
        return new WsDoublewords();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link WsExpectedIncomingData }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/binnatus", name = "WsExpectedIncomingData")
    public JAXBElement<WsExpectedIncomingData> createWsExpectedIncomingData(WsExpectedIncomingData value) {
        return new JAXBElement<WsExpectedIncomingData>(_WsExpectedIncomingData_QNAME, WsExpectedIncomingData.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Filler55 }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/binnatus", name = "Filler55")
    public JAXBElement<Filler55> createFiller55(Filler55 value) {
        return new JAXBElement<Filler55>(_Filler55_QNAME, Filler55 .class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Dfhcommarea }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/binnatus", name = "Dfhcommarea")
    public JAXBElement<Dfhcommarea> createDfhcommarea(Dfhcommarea value) {
        return new JAXBElement<Dfhcommarea>(_Dfhcommarea_QNAME, Dfhcommarea.class, null, value);
    }

}

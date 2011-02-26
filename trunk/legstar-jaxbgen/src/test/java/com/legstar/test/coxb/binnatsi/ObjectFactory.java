
package com.legstar.test.coxb.binnatsi;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.coxb.binnatsi package. 
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

    private final static QName _WsExpectedIncomingData_QNAME = new QName("http://legstar.com/test/coxb/binnatsi", "WsExpectedIncomingData");
    private final static QName _Filler59_QNAME = new QName("http://legstar.com/test/coxb/binnatsi", "Filler59");
    private final static QName _Dfhcommarea_QNAME = new QName("http://legstar.com/test/coxb/binnatsi", "Dfhcommarea");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.coxb.binnatsi
     * 
     */
    public ObjectFactory() {
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
     * Create an instance of {@link WsFullwords }
     * 
     */
    public WsFullwords createWsFullwords() {
        return new WsFullwords();
    }

    /**
     * Create an instance of {@link WsHalfwords }
     * 
     */
    public WsHalfwords createWsHalfwords() {
        return new WsHalfwords();
    }

    /**
     * Create an instance of {@link WsPs9X18MinB }
     * 
     */
    public WsPs9X18MinB createWsPs9X18MinB() {
        return new WsPs9X18MinB();
    }

    /**
     * Create an instance of {@link LsFullwords }
     * 
     */
    public LsFullwords createLsFullwords() {
        return new LsFullwords();
    }

    /**
     * Create an instance of {@link LsHalfwords }
     * 
     */
    public LsHalfwords createLsHalfwords() {
        return new LsHalfwords();
    }

    /**
     * Create an instance of {@link Filler59 }
     * 
     */
    public Filler59 createFiller59() {
        return new Filler59();
    }

    /**
     * Create an instance of {@link WsExpectedIncomingData }
     * 
     */
    public WsExpectedIncomingData createWsExpectedIncomingData() {
        return new WsExpectedIncomingData();
    }

    /**
     * Create an instance of {@link WsPs9X18HighB }
     * 
     */
    public WsPs9X18HighB createWsPs9X18HighB() {
        return new WsPs9X18HighB();
    }

    /**
     * Create an instance of {@link WsUnsignedNative }
     * 
     */
    public WsUnsignedNative createWsUnsignedNative() {
        return new WsUnsignedNative();
    }

    /**
     * Create an instance of {@link Dfhcommarea }
     * 
     */
    public Dfhcommarea createDfhcommarea() {
        return new Dfhcommarea();
    }

    /**
     * Create an instance of {@link LsDoublewords }
     * 
     */
    public LsDoublewords createLsDoublewords() {
        return new LsDoublewords();
    }

    /**
     * Create an instance of {@link WsPs9X18MaxB }
     * 
     */
    public WsPs9X18MaxB createWsPs9X18MaxB() {
        return new WsPs9X18MaxB();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link WsExpectedIncomingData }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/binnatsi", name = "WsExpectedIncomingData")
    public JAXBElement<WsExpectedIncomingData> createWsExpectedIncomingData(WsExpectedIncomingData value) {
        return new JAXBElement<WsExpectedIncomingData>(_WsExpectedIncomingData_QNAME, WsExpectedIncomingData.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Filler59 }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/binnatsi", name = "Filler59")
    public JAXBElement<Filler59> createFiller59(Filler59 value) {
        return new JAXBElement<Filler59>(_Filler59_QNAME, Filler59 .class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Dfhcommarea }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/binnatsi", name = "Dfhcommarea")
    public JAXBElement<Dfhcommarea> createDfhcommarea(Dfhcommarea value) {
        return new JAXBElement<Dfhcommarea>(_Dfhcommarea_QNAME, Dfhcommarea.class, null, value);
    }

}


package com.legstar.test.cixs.lsfileac1;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.lsfileac1 package. 
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

    private final static QName _Lsfileac1HostHeader_QNAME = new QName("http://cixs.test.legstar.com/lsfileac1", "Lsfileac1HostHeader");
    private final static QName _Lsfileac1FaultInfo_QNAME = new QName("http://cixs.test.legstar.com/lsfileac1", "Lsfileac1FaultInfo");
    private final static QName _Lsfileac1Response_QNAME = new QName("http://cixs.test.legstar.com/lsfileac1", "Lsfileac1Response");
    private final static QName _Lsfileac1Request_QNAME = new QName("http://cixs.test.legstar.com/lsfileac1", "Lsfileac1Request");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.lsfileac1
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link Lsfileac1Request }
     * 
     */
    public Lsfileac1Request createLsfileac1Request() {
        return new Lsfileac1Request();
    }

    /**
     * Create an instance of {@link Lsfileac1HostHeader }
     * 
     */
    public Lsfileac1HostHeader createLsfileac1HostHeader() {
        return new Lsfileac1HostHeader();
    }

    /**
     * Create an instance of {@link Lsfileac1FaultInfo }
     * 
     */
    public Lsfileac1FaultInfo createLsfileac1FaultInfo() {
        return new Lsfileac1FaultInfo();
    }

    /**
     * Create an instance of {@link Lsfileac1Response }
     * 
     */
    public Lsfileac1Response createLsfileac1Response() {
        return new Lsfileac1Response();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Lsfileac1HostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileac1", name = "Lsfileac1HostHeader")
    public JAXBElement<Lsfileac1HostHeader> createLsfileac1HostHeader(Lsfileac1HostHeader value) {
        return new JAXBElement<Lsfileac1HostHeader>(_Lsfileac1HostHeader_QNAME, Lsfileac1HostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Lsfileac1FaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileac1", name = "Lsfileac1FaultInfo")
    public JAXBElement<Lsfileac1FaultInfo> createLsfileac1FaultInfo(Lsfileac1FaultInfo value) {
        return new JAXBElement<Lsfileac1FaultInfo>(_Lsfileac1FaultInfo_QNAME, Lsfileac1FaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Lsfileac1Response }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileac1", name = "Lsfileac1Response")
    public JAXBElement<Lsfileac1Response> createLsfileac1Response(Lsfileac1Response value) {
        return new JAXBElement<Lsfileac1Response>(_Lsfileac1Response_QNAME, Lsfileac1Response.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Lsfileac1Request }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileac1", name = "Lsfileac1Request")
    public JAXBElement<Lsfileac1Request> createLsfileac1Request(Lsfileac1Request value) {
        return new JAXBElement<Lsfileac1Request>(_Lsfileac1Request_QNAME, Lsfileac1Request.class, null, value);
    }

}

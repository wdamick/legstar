
package com.legstar.test.cixs.varar021;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.varar021 package. 
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

    private final static QName _Varar021HostHeader_QNAME = new QName("http://cixs.test.legstar.com/varar021", "Varar021HostHeader");
    private final static QName _Varar021FaultInfo_QNAME = new QName("http://cixs.test.legstar.com/varar021", "Varar021FaultInfo");
    private final static QName _Varar021Request_QNAME = new QName("http://cixs.test.legstar.com/varar021", "Varar021Request");
    private final static QName _Varar021Response_QNAME = new QName("http://cixs.test.legstar.com/varar021", "Varar021Response");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.varar021
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link Varar021FaultInfo }
     * 
     */
    public Varar021FaultInfo createVarar021FaultInfo() {
        return new Varar021FaultInfo();
    }

    /**
     * Create an instance of {@link Varar021Response }
     * 
     */
    public Varar021Response createVarar021Response() {
        return new Varar021Response();
    }

    /**
     * Create an instance of {@link Varar021Request }
     * 
     */
    public Varar021Request createVarar021Request() {
        return new Varar021Request();
    }

    /**
     * Create an instance of {@link Varar021HostHeader }
     * 
     */
    public Varar021HostHeader createVarar021HostHeader() {
        return new Varar021HostHeader();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Varar021HostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/varar021", name = "Varar021HostHeader")
    public JAXBElement<Varar021HostHeader> createVarar021HostHeader(Varar021HostHeader value) {
        return new JAXBElement<Varar021HostHeader>(_Varar021HostHeader_QNAME, Varar021HostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Varar021FaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/varar021", name = "Varar021FaultInfo")
    public JAXBElement<Varar021FaultInfo> createVarar021FaultInfo(Varar021FaultInfo value) {
        return new JAXBElement<Varar021FaultInfo>(_Varar021FaultInfo_QNAME, Varar021FaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Varar021Request }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/varar021", name = "Varar021Request")
    public JAXBElement<Varar021Request> createVarar021Request(Varar021Request value) {
        return new JAXBElement<Varar021Request>(_Varar021Request_QNAME, Varar021Request.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Varar021Response }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/varar021", name = "Varar021Response")
    public JAXBElement<Varar021Response> createVarar021Response(Varar021Response value) {
        return new JAXBElement<Varar021Response>(_Varar021Response_QNAME, Varar021Response.class, null, value);
    }

}

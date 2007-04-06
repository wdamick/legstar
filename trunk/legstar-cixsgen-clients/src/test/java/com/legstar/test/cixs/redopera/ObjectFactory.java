
package com.legstar.test.cixs.redopera;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.redopera package. 
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

    private final static QName _RedoperaFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/redopera", "RedoperaFaultInfo");
    private final static QName _RedoperaResponse_QNAME = new QName("http://cixs.test.legstar.com/redopera", "RedoperaResponse");
    private final static QName _RedoperaRequest_QNAME = new QName("http://cixs.test.legstar.com/redopera", "RedoperaRequest");
    private final static QName _HostHeader_QNAME = new QName("http://cixs.test.legstar.com/redopera", "HostHeader");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.redopera
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link RedoperaFaultInfo }
     * 
     */
    public RedoperaFaultInfo createRedoperaFaultInfo() {
        return new RedoperaFaultInfo();
    }

    /**
     * Create an instance of {@link RedoperaResponse }
     * 
     */
    public RedoperaResponse createRedoperaResponse() {
        return new RedoperaResponse();
    }

    /**
     * Create an instance of {@link RedoperaRequest }
     * 
     */
    public RedoperaRequest createRedoperaRequest() {
        return new RedoperaRequest();
    }

    /**
     * Create an instance of {@link RedoperaHostHeader }
     * 
     */
    public RedoperaHostHeader createRedoperaHostHeader() {
        return new RedoperaHostHeader();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedoperaFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redopera", name = "RedoperaFaultInfo")
    public JAXBElement<RedoperaFaultInfo> createRedoperaFaultInfo(RedoperaFaultInfo value) {
        return new JAXBElement<RedoperaFaultInfo>(_RedoperaFaultInfo_QNAME, RedoperaFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedoperaResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redopera", name = "RedoperaResponse")
    public JAXBElement<RedoperaResponse> createRedoperaResponse(RedoperaResponse value) {
        return new JAXBElement<RedoperaResponse>(_RedoperaResponse_QNAME, RedoperaResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedoperaRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redopera", name = "RedoperaRequest")
    public JAXBElement<RedoperaRequest> createRedoperaRequest(RedoperaRequest value) {
        return new JAXBElement<RedoperaRequest>(_RedoperaRequest_QNAME, RedoperaRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedoperaHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redopera", name = "HostHeader")
    public JAXBElement<RedoperaHostHeader> createHostHeader(RedoperaHostHeader value) {
        return new JAXBElement<RedoperaHostHeader>(_HostHeader_QNAME, RedoperaHostHeader.class, null, value);
    }

}

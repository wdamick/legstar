
package com.legstar.test.cixs.redinout;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.redinout package. 
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

    private final static QName _RedinoutResponse_QNAME = new QName("http://cixs.test.legstar.com/redinout", "RedinoutResponse");
    private final static QName _RedinoutFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/redinout", "RedinoutFaultInfo");
    private final static QName _HostHeader_QNAME = new QName("http://cixs.test.legstar.com/redinout", "HostHeader");
    private final static QName _RedinoutRequest_QNAME = new QName("http://cixs.test.legstar.com/redinout", "RedinoutRequest");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.redinout
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link RedinoutResponse }
     * 
     */
    public RedinoutResponse createRedinoutResponse() {
        return new RedinoutResponse();
    }

    /**
     * Create an instance of {@link RedinoutHostHeader }
     * 
     */
    public RedinoutHostHeader createRedinoutHostHeader() {
        return new RedinoutHostHeader();
    }

    /**
     * Create an instance of {@link RedinoutFaultInfo }
     * 
     */
    public RedinoutFaultInfo createRedinoutFaultInfo() {
        return new RedinoutFaultInfo();
    }

    /**
     * Create an instance of {@link RedinoutRequest }
     * 
     */
    public RedinoutRequest createRedinoutRequest() {
        return new RedinoutRequest();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedinoutResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redinout", name = "RedinoutResponse")
    public JAXBElement<RedinoutResponse> createRedinoutResponse(RedinoutResponse value) {
        return new JAXBElement<RedinoutResponse>(_RedinoutResponse_QNAME, RedinoutResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedinoutFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redinout", name = "RedinoutFaultInfo")
    public JAXBElement<RedinoutFaultInfo> createRedinoutFaultInfo(RedinoutFaultInfo value) {
        return new JAXBElement<RedinoutFaultInfo>(_RedinoutFaultInfo_QNAME, RedinoutFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedinoutHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redinout", name = "HostHeader")
    public JAXBElement<RedinoutHostHeader> createHostHeader(RedinoutHostHeader value) {
        return new JAXBElement<RedinoutHostHeader>(_HostHeader_QNAME, RedinoutHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedinoutRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redinout", name = "RedinoutRequest")
    public JAXBElement<RedinoutRequest> createRedinoutRequest(RedinoutRequest value) {
        return new JAXBElement<RedinoutRequest>(_RedinoutRequest_QNAME, RedinoutRequest.class, null, value);
    }

}

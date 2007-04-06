
package com.legstar.test.cixs.fixarnum;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.fixarnum package. 
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

    private final static QName _FixarnumFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/fixarnum", "FixarnumFaultInfo");
    private final static QName _FixarnumRequest_QNAME = new QName("http://cixs.test.legstar.com/fixarnum", "FixarnumRequest");
    private final static QName _HostHeader_QNAME = new QName("http://cixs.test.legstar.com/fixarnum", "HostHeader");
    private final static QName _FixarnumResponse_QNAME = new QName("http://cixs.test.legstar.com/fixarnum", "FixarnumResponse");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.fixarnum
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link FixarnumHostHeader }
     * 
     */
    public FixarnumHostHeader createFixarnumHostHeader() {
        return new FixarnumHostHeader();
    }

    /**
     * Create an instance of {@link FixarnumFaultInfo }
     * 
     */
    public FixarnumFaultInfo createFixarnumFaultInfo() {
        return new FixarnumFaultInfo();
    }

    /**
     * Create an instance of {@link FixarnumRequest }
     * 
     */
    public FixarnumRequest createFixarnumRequest() {
        return new FixarnumRequest();
    }

    /**
     * Create an instance of {@link FixarnumResponse }
     * 
     */
    public FixarnumResponse createFixarnumResponse() {
        return new FixarnumResponse();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link FixarnumFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/fixarnum", name = "FixarnumFaultInfo")
    public JAXBElement<FixarnumFaultInfo> createFixarnumFaultInfo(FixarnumFaultInfo value) {
        return new JAXBElement<FixarnumFaultInfo>(_FixarnumFaultInfo_QNAME, FixarnumFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link FixarnumRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/fixarnum", name = "FixarnumRequest")
    public JAXBElement<FixarnumRequest> createFixarnumRequest(FixarnumRequest value) {
        return new JAXBElement<FixarnumRequest>(_FixarnumRequest_QNAME, FixarnumRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link FixarnumHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/fixarnum", name = "HostHeader")
    public JAXBElement<FixarnumHostHeader> createHostHeader(FixarnumHostHeader value) {
        return new JAXBElement<FixarnumHostHeader>(_HostHeader_QNAME, FixarnumHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link FixarnumResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/fixarnum", name = "FixarnumResponse")
    public JAXBElement<FixarnumResponse> createFixarnumResponse(FixarnumResponse value) {
        return new JAXBElement<FixarnumResponse>(_FixarnumResponse_QNAME, FixarnumResponse.class, null, value);
    }

}


package com.legstar.test.cixs.fixarcom;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.fixarcom package. 
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

    private final static QName _FixarcomFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/fixarcom", "FixarcomFaultInfo");
    private final static QName _HostHeader_QNAME = new QName("http://cixs.test.legstar.com/fixarcom", "HostHeader");
    private final static QName _FixarcomRequest_QNAME = new QName("http://cixs.test.legstar.com/fixarcom", "FixarcomRequest");
    private final static QName _FixarcomResponse_QNAME = new QName("http://cixs.test.legstar.com/fixarcom", "FixarcomResponse");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.fixarcom
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link FixarcomResponse }
     * 
     */
    public FixarcomResponse createFixarcomResponse() {
        return new FixarcomResponse();
    }

    /**
     * Create an instance of {@link FixarcomRequest }
     * 
     */
    public FixarcomRequest createFixarcomRequest() {
        return new FixarcomRequest();
    }

    /**
     * Create an instance of {@link FixarcomHostHeader }
     * 
     */
    public FixarcomHostHeader createFixarcomHostHeader() {
        return new FixarcomHostHeader();
    }

    /**
     * Create an instance of {@link FixarcomFaultInfo }
     * 
     */
    public FixarcomFaultInfo createFixarcomFaultInfo() {
        return new FixarcomFaultInfo();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link FixarcomFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/fixarcom", name = "FixarcomFaultInfo")
    public JAXBElement<FixarcomFaultInfo> createFixarcomFaultInfo(FixarcomFaultInfo value) {
        return new JAXBElement<FixarcomFaultInfo>(_FixarcomFaultInfo_QNAME, FixarcomFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link FixarcomHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/fixarcom", name = "HostHeader")
    public JAXBElement<FixarcomHostHeader> createHostHeader(FixarcomHostHeader value) {
        return new JAXBElement<FixarcomHostHeader>(_HostHeader_QNAME, FixarcomHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link FixarcomRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/fixarcom", name = "FixarcomRequest")
    public JAXBElement<FixarcomRequest> createFixarcomRequest(FixarcomRequest value) {
        return new JAXBElement<FixarcomRequest>(_FixarcomRequest_QNAME, FixarcomRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link FixarcomResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/fixarcom", name = "FixarcomResponse")
    public JAXBElement<FixarcomResponse> createFixarcomResponse(FixarcomResponse value) {
        return new JAXBElement<FixarcomResponse>(_FixarcomResponse_QNAME, FixarcomResponse.class, null, value);
    }

}

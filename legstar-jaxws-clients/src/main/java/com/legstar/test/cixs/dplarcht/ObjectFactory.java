
package com.legstar.test.cixs.dplarcht;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.dplarcht package. 
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

    private final static QName _DplarchtFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/dplarcht", "DplarchtFaultInfo");
    private final static QName _DplarchtHostHeader_QNAME = new QName("http://cixs.test.legstar.com/dplarcht", "DplarchtHostHeader");
    private final static QName _DplarchtRequest_QNAME = new QName("http://cixs.test.legstar.com/dplarcht", "DplarchtRequest");
    private final static QName _DplarchtResponse_QNAME = new QName("http://cixs.test.legstar.com/dplarcht", "DplarchtResponse");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.dplarcht
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link DplarchtHostHeader }
     * 
     */
    public DplarchtHostHeader createDplarchtHostHeader() {
        return new DplarchtHostHeader();
    }

    /**
     * Create an instance of {@link DplarchtRequest }
     * 
     */
    public DplarchtRequest createDplarchtRequest() {
        return new DplarchtRequest();
    }

    /**
     * Create an instance of {@link DplarchtResponse }
     * 
     */
    public DplarchtResponse createDplarchtResponse() {
        return new DplarchtResponse();
    }

    /**
     * Create an instance of {@link DplarchtFaultInfo }
     * 
     */
    public DplarchtFaultInfo createDplarchtFaultInfo() {
        return new DplarchtFaultInfo();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link DplarchtFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/dplarcht", name = "DplarchtFaultInfo")
    public JAXBElement<DplarchtFaultInfo> createDplarchtFaultInfo(DplarchtFaultInfo value) {
        return new JAXBElement<DplarchtFaultInfo>(_DplarchtFaultInfo_QNAME, DplarchtFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link DplarchtHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/dplarcht", name = "DplarchtHostHeader")
    public JAXBElement<DplarchtHostHeader> createDplarchtHostHeader(DplarchtHostHeader value) {
        return new JAXBElement<DplarchtHostHeader>(_DplarchtHostHeader_QNAME, DplarchtHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link DplarchtRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/dplarcht", name = "DplarchtRequest")
    public JAXBElement<DplarchtRequest> createDplarchtRequest(DplarchtRequest value) {
        return new JAXBElement<DplarchtRequest>(_DplarchtRequest_QNAME, DplarchtRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link DplarchtResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/dplarcht", name = "DplarchtResponse")
    public JAXBElement<DplarchtResponse> createDplarchtResponse(DplarchtResponse value) {
        return new JAXBElement<DplarchtResponse>(_DplarchtResponse_QNAME, DplarchtResponse.class, null, value);
    }

}

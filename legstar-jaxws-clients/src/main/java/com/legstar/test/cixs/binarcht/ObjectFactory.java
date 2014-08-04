
package com.legstar.test.cixs.binarcht;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.binarcht package. 
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

    private final static QName _BinarchtFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/binarcht", "BinarchtFaultInfo");
    private final static QName _BinarchtRequest_QNAME = new QName("http://cixs.test.legstar.com/binarcht", "BinarchtRequest");
    private final static QName _BinarchtResponse_QNAME = new QName("http://cixs.test.legstar.com/binarcht", "BinarchtResponse");
    private final static QName _BinarchtHostHeader_QNAME = new QName("http://cixs.test.legstar.com/binarcht", "BinarchtHostHeader");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.binarcht
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link BinarchtRequest }
     * 
     */
    public BinarchtRequest createBinarchtRequest() {
        return new BinarchtRequest();
    }

    /**
     * Create an instance of {@link BinarchtHostHeader }
     * 
     */
    public BinarchtHostHeader createBinarchtHostHeader() {
        return new BinarchtHostHeader();
    }

    /**
     * Create an instance of {@link BinarchtFaultInfo }
     * 
     */
    public BinarchtFaultInfo createBinarchtFaultInfo() {
        return new BinarchtFaultInfo();
    }

    /**
     * Create an instance of {@link BinarchtResponse }
     * 
     */
    public BinarchtResponse createBinarchtResponse() {
        return new BinarchtResponse();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinarchtFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binarcht", name = "BinarchtFaultInfo")
    public JAXBElement<BinarchtFaultInfo> createBinarchtFaultInfo(BinarchtFaultInfo value) {
        return new JAXBElement<BinarchtFaultInfo>(_BinarchtFaultInfo_QNAME, BinarchtFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinarchtRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binarcht", name = "BinarchtRequest")
    public JAXBElement<BinarchtRequest> createBinarchtRequest(BinarchtRequest value) {
        return new JAXBElement<BinarchtRequest>(_BinarchtRequest_QNAME, BinarchtRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinarchtResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binarcht", name = "BinarchtResponse")
    public JAXBElement<BinarchtResponse> createBinarchtResponse(BinarchtResponse value) {
        return new JAXBElement<BinarchtResponse>(_BinarchtResponse_QNAME, BinarchtResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinarchtHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binarcht", name = "BinarchtHostHeader")
    public JAXBElement<BinarchtHostHeader> createBinarchtHostHeader(BinarchtHostHeader value) {
        return new JAXBElement<BinarchtHostHeader>(_BinarchtHostHeader_QNAME, BinarchtHostHeader.class, null, value);
    }

}

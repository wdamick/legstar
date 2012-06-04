
package com.legstar.test.cixs.binpkdus;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.binpkdus package. 
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

    private final static QName _BinpkdusFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/binpkdus", "BinpkdusFaultInfo");
    private final static QName _BinpkdusResponse_QNAME = new QName("http://cixs.test.legstar.com/binpkdus", "BinpkdusResponse");
    private final static QName _BinpkdusHostHeader_QNAME = new QName("http://cixs.test.legstar.com/binpkdus", "BinpkdusHostHeader");
    private final static QName _BinpkdusRequest_QNAME = new QName("http://cixs.test.legstar.com/binpkdus", "BinpkdusRequest");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.binpkdus
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link BinpkdusResponse }
     * 
     */
    public BinpkdusResponse createBinpkdusResponse() {
        return new BinpkdusResponse();
    }

    /**
     * Create an instance of {@link BinpkdusHostHeader }
     * 
     */
    public BinpkdusHostHeader createBinpkdusHostHeader() {
        return new BinpkdusHostHeader();
    }

    /**
     * Create an instance of {@link BinpkdusRequest }
     * 
     */
    public BinpkdusRequest createBinpkdusRequest() {
        return new BinpkdusRequest();
    }

    /**
     * Create an instance of {@link BinpkdusFaultInfo }
     * 
     */
    public BinpkdusFaultInfo createBinpkdusFaultInfo() {
        return new BinpkdusFaultInfo();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinpkdusFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binpkdus", name = "BinpkdusFaultInfo")
    public JAXBElement<BinpkdusFaultInfo> createBinpkdusFaultInfo(BinpkdusFaultInfo value) {
        return new JAXBElement<BinpkdusFaultInfo>(_BinpkdusFaultInfo_QNAME, BinpkdusFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinpkdusResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binpkdus", name = "BinpkdusResponse")
    public JAXBElement<BinpkdusResponse> createBinpkdusResponse(BinpkdusResponse value) {
        return new JAXBElement<BinpkdusResponse>(_BinpkdusResponse_QNAME, BinpkdusResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinpkdusHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binpkdus", name = "BinpkdusHostHeader")
    public JAXBElement<BinpkdusHostHeader> createBinpkdusHostHeader(BinpkdusHostHeader value) {
        return new JAXBElement<BinpkdusHostHeader>(_BinpkdusHostHeader_QNAME, BinpkdusHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinpkdusRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binpkdus", name = "BinpkdusRequest")
    public JAXBElement<BinpkdusRequest> createBinpkdusRequest(BinpkdusRequest value) {
        return new JAXBElement<BinpkdusRequest>(_BinpkdusRequest_QNAME, BinpkdusRequest.class, null, value);
    }

}

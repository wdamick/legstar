
package com.legstar.test.cixs.binnatus;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.binnatus package. 
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

    private final static QName _BinnatusRequest_QNAME = new QName("http://cixs.test.legstar.com/binnatus", "BinnatusRequest");
    private final static QName _HostHeader_QNAME = new QName("http://cixs.test.legstar.com/binnatus", "HostHeader");
    private final static QName _BinnatusFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/binnatus", "BinnatusFaultInfo");
    private final static QName _BinnatusResponse_QNAME = new QName("http://cixs.test.legstar.com/binnatus", "BinnatusResponse");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.binnatus
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link BinnatusHostHeader }
     * 
     */
    public BinnatusHostHeader createBinnatusHostHeader() {
        return new BinnatusHostHeader();
    }

    /**
     * Create an instance of {@link BinnatusFaultInfo }
     * 
     */
    public BinnatusFaultInfo createBinnatusFaultInfo() {
        return new BinnatusFaultInfo();
    }

    /**
     * Create an instance of {@link BinnatusResponse }
     * 
     */
    public BinnatusResponse createBinnatusResponse() {
        return new BinnatusResponse();
    }

    /**
     * Create an instance of {@link BinnatusRequest }
     * 
     */
    public BinnatusRequest createBinnatusRequest() {
        return new BinnatusRequest();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinnatusRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binnatus", name = "BinnatusRequest")
    public JAXBElement<BinnatusRequest> createBinnatusRequest(BinnatusRequest value) {
        return new JAXBElement<BinnatusRequest>(_BinnatusRequest_QNAME, BinnatusRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinnatusHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binnatus", name = "HostHeader")
    public JAXBElement<BinnatusHostHeader> createHostHeader(BinnatusHostHeader value) {
        return new JAXBElement<BinnatusHostHeader>(_HostHeader_QNAME, BinnatusHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinnatusFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binnatus", name = "BinnatusFaultInfo")
    public JAXBElement<BinnatusFaultInfo> createBinnatusFaultInfo(BinnatusFaultInfo value) {
        return new JAXBElement<BinnatusFaultInfo>(_BinnatusFaultInfo_QNAME, BinnatusFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinnatusResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binnatus", name = "BinnatusResponse")
    public JAXBElement<BinnatusResponse> createBinnatusResponse(BinnatusResponse value) {
        return new JAXBElement<BinnatusResponse>(_BinnatusResponse_QNAME, BinnatusResponse.class, null, value);
    }

}


package com.legstar.test.cixs.binnatsi;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.binnatsi package. 
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

    private final static QName _BinnatsiHostHeader_QNAME = new QName("http://cixs.test.legstar.com/binnatsi", "BinnatsiHostHeader");
    private final static QName _BinnatsiResponse_QNAME = new QName("http://cixs.test.legstar.com/binnatsi", "BinnatsiResponse");
    private final static QName _BinnatsiFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/binnatsi", "BinnatsiFaultInfo");
    private final static QName _BinnatsiRequest_QNAME = new QName("http://cixs.test.legstar.com/binnatsi", "BinnatsiRequest");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.binnatsi
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link BinnatsiFaultInfo }
     * 
     */
    public BinnatsiFaultInfo createBinnatsiFaultInfo() {
        return new BinnatsiFaultInfo();
    }

    /**
     * Create an instance of {@link BinnatsiResponse }
     * 
     */
    public BinnatsiResponse createBinnatsiResponse() {
        return new BinnatsiResponse();
    }

    /**
     * Create an instance of {@link BinnatsiHostHeader }
     * 
     */
    public BinnatsiHostHeader createBinnatsiHostHeader() {
        return new BinnatsiHostHeader();
    }

    /**
     * Create an instance of {@link BinnatsiRequest }
     * 
     */
    public BinnatsiRequest createBinnatsiRequest() {
        return new BinnatsiRequest();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinnatsiHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binnatsi", name = "BinnatsiHostHeader")
    public JAXBElement<BinnatsiHostHeader> createBinnatsiHostHeader(BinnatsiHostHeader value) {
        return new JAXBElement<BinnatsiHostHeader>(_BinnatsiHostHeader_QNAME, BinnatsiHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinnatsiResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binnatsi", name = "BinnatsiResponse")
    public JAXBElement<BinnatsiResponse> createBinnatsiResponse(BinnatsiResponse value) {
        return new JAXBElement<BinnatsiResponse>(_BinnatsiResponse_QNAME, BinnatsiResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinnatsiFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binnatsi", name = "BinnatsiFaultInfo")
    public JAXBElement<BinnatsiFaultInfo> createBinnatsiFaultInfo(BinnatsiFaultInfo value) {
        return new JAXBElement<BinnatsiFaultInfo>(_BinnatsiFaultInfo_QNAME, BinnatsiFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link BinnatsiRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/binnatsi", name = "BinnatsiRequest")
    public JAXBElement<BinnatsiRequest> createBinnatsiRequest(BinnatsiRequest value) {
        return new JAXBElement<BinnatsiRequest>(_BinnatsiRequest_QNAME, BinnatsiRequest.class, null, value);
    }

}

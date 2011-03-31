
package com.legstar.test.cixs.redsimpt;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.redsimpt package. 
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

    private final static QName _RedsimptResponse_QNAME = new QName("http://cixs.test.legstar.com/redsimpt", "RedsimptResponse");
    private final static QName _RedsimptRequest_QNAME = new QName("http://cixs.test.legstar.com/redsimpt", "RedsimptRequest");
    private final static QName _RedsimptHostHeader_QNAME = new QName("http://cixs.test.legstar.com/redsimpt", "RedsimptHostHeader");
    private final static QName _RedsimptFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/redsimpt", "RedsimptFaultInfo");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.redsimpt
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link RedsimptHostHeader }
     * 
     */
    public RedsimptHostHeader createRedsimptHostHeader() {
        return new RedsimptHostHeader();
    }

    /**
     * Create an instance of {@link RedsimptFaultInfo }
     * 
     */
    public RedsimptFaultInfo createRedsimptFaultInfo() {
        return new RedsimptFaultInfo();
    }

    /**
     * Create an instance of {@link RedsimptRequest }
     * 
     */
    public RedsimptRequest createRedsimptRequest() {
        return new RedsimptRequest();
    }

    /**
     * Create an instance of {@link RedsimptResponse }
     * 
     */
    public RedsimptResponse createRedsimptResponse() {
        return new RedsimptResponse();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedsimptResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redsimpt", name = "RedsimptResponse")
    public JAXBElement<RedsimptResponse> createRedsimptResponse(RedsimptResponse value) {
        return new JAXBElement<RedsimptResponse>(_RedsimptResponse_QNAME, RedsimptResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedsimptRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redsimpt", name = "RedsimptRequest")
    public JAXBElement<RedsimptRequest> createRedsimptRequest(RedsimptRequest value) {
        return new JAXBElement<RedsimptRequest>(_RedsimptRequest_QNAME, RedsimptRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedsimptHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redsimpt", name = "RedsimptHostHeader")
    public JAXBElement<RedsimptHostHeader> createRedsimptHostHeader(RedsimptHostHeader value) {
        return new JAXBElement<RedsimptHostHeader>(_RedsimptHostHeader_QNAME, RedsimptHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RedsimptFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/redsimpt", name = "RedsimptFaultInfo")
    public JAXBElement<RedsimptFaultInfo> createRedsimptFaultInfo(RedsimptFaultInfo value) {
        return new JAXBElement<RedsimptFaultInfo>(_RedsimptFaultInfo_QNAME, RedsimptFaultInfo.class, null, value);
    }

}

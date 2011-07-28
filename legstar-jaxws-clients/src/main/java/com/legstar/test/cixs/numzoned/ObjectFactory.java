
package com.legstar.test.cixs.numzoned;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.numzoned package. 
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

    private final static QName _NumzonedHostHeader_QNAME = new QName("http://cixs.test.legstar.com/numzoned", "NumzonedHostHeader");
    private final static QName _NumzonedRequest_QNAME = new QName("http://cixs.test.legstar.com/numzoned", "NumzonedRequest");
    private final static QName _NumzonedFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/numzoned", "NumzonedFaultInfo");
    private final static QName _NumzonedResponse_QNAME = new QName("http://cixs.test.legstar.com/numzoned", "NumzonedResponse");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.numzoned
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link NumzonedHostHeader }
     * 
     */
    public NumzonedHostHeader createNumzonedHostHeader() {
        return new NumzonedHostHeader();
    }

    /**
     * Create an instance of {@link NumzonedResponse }
     * 
     */
    public NumzonedResponse createNumzonedResponse() {
        return new NumzonedResponse();
    }

    /**
     * Create an instance of {@link NumzonedFaultInfo }
     * 
     */
    public NumzonedFaultInfo createNumzonedFaultInfo() {
        return new NumzonedFaultInfo();
    }

    /**
     * Create an instance of {@link NumzonedRequest }
     * 
     */
    public NumzonedRequest createNumzonedRequest() {
        return new NumzonedRequest();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link NumzonedHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/numzoned", name = "NumzonedHostHeader")
    public JAXBElement<NumzonedHostHeader> createNumzonedHostHeader(NumzonedHostHeader value) {
        return new JAXBElement<NumzonedHostHeader>(_NumzonedHostHeader_QNAME, NumzonedHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link NumzonedRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/numzoned", name = "NumzonedRequest")
    public JAXBElement<NumzonedRequest> createNumzonedRequest(NumzonedRequest value) {
        return new JAXBElement<NumzonedRequest>(_NumzonedRequest_QNAME, NumzonedRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link NumzonedFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/numzoned", name = "NumzonedFaultInfo")
    public JAXBElement<NumzonedFaultInfo> createNumzonedFaultInfo(NumzonedFaultInfo value) {
        return new JAXBElement<NumzonedFaultInfo>(_NumzonedFaultInfo_QNAME, NumzonedFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link NumzonedResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/numzoned", name = "NumzonedResponse")
    public JAXBElement<NumzonedResponse> createNumzonedResponse(NumzonedResponse value) {
        return new JAXBElement<NumzonedResponse>(_NumzonedResponse_QNAME, NumzonedResponse.class, null, value);
    }

}


package com.legstar.test.cixs.fixarsim;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.fixarsim package. 
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

    private final static QName _FixarsimResponse_QNAME = new QName("http://cixs.test.legstar.com/fixarsim", "FixarsimResponse");
    private final static QName _FixarsimHostHeader_QNAME = new QName("http://cixs.test.legstar.com/fixarsim", "FixarsimHostHeader");
    private final static QName _FixarsimFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/fixarsim", "FixarsimFaultInfo");
    private final static QName _FixarsimRequest_QNAME = new QName("http://cixs.test.legstar.com/fixarsim", "FixarsimRequest");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.fixarsim
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link FixarsimRequest }
     * 
     */
    public FixarsimRequest createFixarsimRequest() {
        return new FixarsimRequest();
    }

    /**
     * Create an instance of {@link FixarsimFaultInfo }
     * 
     */
    public FixarsimFaultInfo createFixarsimFaultInfo() {
        return new FixarsimFaultInfo();
    }

    /**
     * Create an instance of {@link FixarsimHostHeader }
     * 
     */
    public FixarsimHostHeader createFixarsimHostHeader() {
        return new FixarsimHostHeader();
    }

    /**
     * Create an instance of {@link FixarsimResponse }
     * 
     */
    public FixarsimResponse createFixarsimResponse() {
        return new FixarsimResponse();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link FixarsimResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/fixarsim", name = "FixarsimResponse")
    public JAXBElement<FixarsimResponse> createFixarsimResponse(FixarsimResponse value) {
        return new JAXBElement<FixarsimResponse>(_FixarsimResponse_QNAME, FixarsimResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link FixarsimHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/fixarsim", name = "FixarsimHostHeader")
    public JAXBElement<FixarsimHostHeader> createFixarsimHostHeader(FixarsimHostHeader value) {
        return new JAXBElement<FixarsimHostHeader>(_FixarsimHostHeader_QNAME, FixarsimHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link FixarsimFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/fixarsim", name = "FixarsimFaultInfo")
    public JAXBElement<FixarsimFaultInfo> createFixarsimFaultInfo(FixarsimFaultInfo value) {
        return new JAXBElement<FixarsimFaultInfo>(_FixarsimFaultInfo_QNAME, FixarsimFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link FixarsimRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/fixarsim", name = "FixarsimRequest")
    public JAXBElement<FixarsimRequest> createFixarsimRequest(FixarsimRequest value) {
        return new JAXBElement<FixarsimRequest>(_FixarsimRequest_QNAME, FixarsimRequest.class, null, value);
    }

}

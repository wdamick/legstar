
package com.legstar.test.cixs.lsfilead;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.lsfilead package. 
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

    private final static QName _HostHeader_QNAME = new QName("http://cixs.test.legstar.com/lsfilead", "HostHeader");
    private final static QName _LsfileadResponse_QNAME = new QName("http://cixs.test.legstar.com/lsfilead", "LsfileadResponse");
    private final static QName _LsfileadRequest_QNAME = new QName("http://cixs.test.legstar.com/lsfilead", "LsfileadRequest");
    private final static QName _LsfileadFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/lsfilead", "LsfileadFaultInfo");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.lsfilead
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link LsfileadRequest }
     * 
     */
    public LsfileadRequest createLsfileadRequest() {
        return new LsfileadRequest();
    }

    /**
     * Create an instance of {@link LsfileadFaultInfo }
     * 
     */
    public LsfileadFaultInfo createLsfileadFaultInfo() {
        return new LsfileadFaultInfo();
    }

    /**
     * Create an instance of {@link LsfileadResponse }
     * 
     */
    public LsfileadResponse createLsfileadResponse() {
        return new LsfileadResponse();
    }

    /**
     * Create an instance of {@link LsfileadHostHeader }
     * 
     */
    public LsfileadHostHeader createLsfileadHostHeader() {
        return new LsfileadHostHeader();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileadHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfilead", name = "HostHeader")
    public JAXBElement<LsfileadHostHeader> createHostHeader(LsfileadHostHeader value) {
        return new JAXBElement<LsfileadHostHeader>(_HostHeader_QNAME, LsfileadHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileadResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfilead", name = "LsfileadResponse")
    public JAXBElement<LsfileadResponse> createLsfileadResponse(LsfileadResponse value) {
        return new JAXBElement<LsfileadResponse>(_LsfileadResponse_QNAME, LsfileadResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileadRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfilead", name = "LsfileadRequest")
    public JAXBElement<LsfileadRequest> createLsfileadRequest(LsfileadRequest value) {
        return new JAXBElement<LsfileadRequest>(_LsfileadRequest_QNAME, LsfileadRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileadFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfilead", name = "LsfileadFaultInfo")
    public JAXBElement<LsfileadFaultInfo> createLsfileadFaultInfo(LsfileadFaultInfo value) {
        return new JAXBElement<LsfileadFaultInfo>(_LsfileadFaultInfo_QNAME, LsfileadFaultInfo.class, null, value);
    }

}

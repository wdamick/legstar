
package com.legstar.test.cixs.lsfileaq;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.lsfileaq package. 
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

    private final static QName _LsfileaqHostHeader_QNAME = new QName("http://cixs.test.legstar.com/lsfileaq", "LsfileaqHostHeader");
    private final static QName _LsfileaqResponse_QNAME = new QName("http://cixs.test.legstar.com/lsfileaq", "LsfileaqResponse");
    private final static QName _LsfileaqFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/lsfileaq", "LsfileaqFaultInfo");
    private final static QName _LsfileaqRequest_QNAME = new QName("http://cixs.test.legstar.com/lsfileaq", "LsfileaqRequest");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.lsfileaq
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link LsfileaqFaultInfo }
     * 
     */
    public LsfileaqFaultInfo createLsfileaqFaultInfo() {
        return new LsfileaqFaultInfo();
    }

    /**
     * Create an instance of {@link LsfileaqRequest }
     * 
     */
    public LsfileaqRequest createLsfileaqRequest() {
        return new LsfileaqRequest();
    }

    /**
     * Create an instance of {@link LsfileaqHostHeader }
     * 
     */
    public LsfileaqHostHeader createLsfileaqHostHeader() {
        return new LsfileaqHostHeader();
    }

    /**
     * Create an instance of {@link LsfileaqResponse }
     * 
     */
    public LsfileaqResponse createLsfileaqResponse() {
        return new LsfileaqResponse();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileaqHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileaq", name = "LsfileaqHostHeader")
    public JAXBElement<LsfileaqHostHeader> createLsfileaqHostHeader(LsfileaqHostHeader value) {
        return new JAXBElement<LsfileaqHostHeader>(_LsfileaqHostHeader_QNAME, LsfileaqHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileaqResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileaq", name = "LsfileaqResponse")
    public JAXBElement<LsfileaqResponse> createLsfileaqResponse(LsfileaqResponse value) {
        return new JAXBElement<LsfileaqResponse>(_LsfileaqResponse_QNAME, LsfileaqResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileaqFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileaq", name = "LsfileaqFaultInfo")
    public JAXBElement<LsfileaqFaultInfo> createLsfileaqFaultInfo(LsfileaqFaultInfo value) {
        return new JAXBElement<LsfileaqFaultInfo>(_LsfileaqFaultInfo_QNAME, LsfileaqFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileaqRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileaq", name = "LsfileaqRequest")
    public JAXBElement<LsfileaqRequest> createLsfileaqRequest(LsfileaqRequest value) {
        return new JAXBElement<LsfileaqRequest>(_LsfileaqRequest_QNAME, LsfileaqRequest.class, null, value);
    }

}

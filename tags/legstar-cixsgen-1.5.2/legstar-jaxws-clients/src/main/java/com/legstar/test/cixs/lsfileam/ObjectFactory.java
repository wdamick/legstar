
package com.legstar.test.cixs.lsfileam;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.lsfileam package. 
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

    private final static QName _LsfileamHostHeader_QNAME = new QName("http://cixs.test.legstar.com/lsfileam", "LsfileamHostHeader");
    private final static QName _LsfileamRequestHolder_QNAME = new QName("http://cixs.test.legstar.com/lsfileam", "LsfileamRequestHolder");
    private final static QName _LsfileamResponseHolder_QNAME = new QName("http://cixs.test.legstar.com/lsfileam", "LsfileamResponseHolder");
    private final static QName _LsfileamRequest_QNAME = new QName("http://cixs.test.legstar.com/lsfileam", "LsfileamRequest");
    private final static QName _LsfileamFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/lsfileam", "LsfileamFaultInfo");
    private final static QName _LsfileamResponse_QNAME = new QName("http://cixs.test.legstar.com/lsfileam", "LsfileamResponse");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.lsfileam
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link LsfileamRequestHolder }
     * 
     */
    public LsfileamRequestHolder createLsfileamRequestHolder() {
        return new LsfileamRequestHolder();
    }

    /**
     * Create an instance of {@link LsfileamResponseHolder }
     * 
     */
    public LsfileamResponseHolder createLsfileamResponseHolder() {
        return new LsfileamResponseHolder();
    }

    /**
     * Create an instance of {@link LsfileamResponse }
     * 
     */
    public LsfileamResponse createLsfileamResponse() {
        return new LsfileamResponse();
    }

    /**
     * Create an instance of {@link LsfileamFaultInfo }
     * 
     */
    public LsfileamFaultInfo createLsfileamFaultInfo() {
        return new LsfileamFaultInfo();
    }

    /**
     * Create an instance of {@link LsfileamHostHeader }
     * 
     */
    public LsfileamHostHeader createLsfileamHostHeader() {
        return new LsfileamHostHeader();
    }

    /**
     * Create an instance of {@link LsfileamRequest }
     * 
     */
    public LsfileamRequest createLsfileamRequest() {
        return new LsfileamRequest();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileamHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileam", name = "LsfileamHostHeader")
    public JAXBElement<LsfileamHostHeader> createLsfileamHostHeader(LsfileamHostHeader value) {
        return new JAXBElement<LsfileamHostHeader>(_LsfileamHostHeader_QNAME, LsfileamHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileamRequestHolder }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileam", name = "LsfileamRequestHolder")
    public JAXBElement<LsfileamRequestHolder> createLsfileamRequestHolder(LsfileamRequestHolder value) {
        return new JAXBElement<LsfileamRequestHolder>(_LsfileamRequestHolder_QNAME, LsfileamRequestHolder.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileamResponseHolder }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileam", name = "LsfileamResponseHolder")
    public JAXBElement<LsfileamResponseHolder> createLsfileamResponseHolder(LsfileamResponseHolder value) {
        return new JAXBElement<LsfileamResponseHolder>(_LsfileamResponseHolder_QNAME, LsfileamResponseHolder.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileamRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileam", name = "LsfileamRequest")
    public JAXBElement<LsfileamRequest> createLsfileamRequest(LsfileamRequest value) {
        return new JAXBElement<LsfileamRequest>(_LsfileamRequest_QNAME, LsfileamRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileamFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileam", name = "LsfileamFaultInfo")
    public JAXBElement<LsfileamFaultInfo> createLsfileamFaultInfo(LsfileamFaultInfo value) {
        return new JAXBElement<LsfileamFaultInfo>(_LsfileamFaultInfo_QNAME, LsfileamFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileamResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileam", name = "LsfileamResponse")
    public JAXBElement<LsfileamResponse> createLsfileamResponse(LsfileamResponse value) {
        return new JAXBElement<LsfileamResponse>(_LsfileamResponse_QNAME, LsfileamResponse.class, null, value);
    }

}


package com.legstar.test.cixs.lsfileac;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.lsfileac package. 
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

    private final static QName _LsfileacFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/lsfileac", "LsfileacFaultInfo");
    private final static QName _LsfileacRequest_QNAME = new QName("http://cixs.test.legstar.com/lsfileac", "LsfileacRequest");
    private final static QName _LsfileacHostHeader_QNAME = new QName("http://cixs.test.legstar.com/lsfileac", "LsfileacHostHeader");
    private final static QName _LsfileacRequestHolder_QNAME = new QName("http://cixs.test.legstar.com/lsfileac", "LsfileacRequestHolder");
    private final static QName _LsfileacResponseHolder_QNAME = new QName("http://cixs.test.legstar.com/lsfileac", "LsfileacResponseHolder");
    private final static QName _LsfileacResponse_QNAME = new QName("http://cixs.test.legstar.com/lsfileac", "LsfileacResponse");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.lsfileac
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link LsfileacRequest }
     * 
     */
    public LsfileacRequest createLsfileacRequest() {
        return new LsfileacRequest();
    }

    /**
     * Create an instance of {@link LsfileacRequestHolder }
     * 
     */
    public LsfileacRequestHolder createLsfileacRequestHolder() {
        return new LsfileacRequestHolder();
    }

    /**
     * Create an instance of {@link LsfileacResponse }
     * 
     */
    public LsfileacResponse createLsfileacResponse() {
        return new LsfileacResponse();
    }

    /**
     * Create an instance of {@link LsfileacFaultInfo }
     * 
     */
    public LsfileacFaultInfo createLsfileacFaultInfo() {
        return new LsfileacFaultInfo();
    }

    /**
     * Create an instance of {@link LsfileacResponseHolder }
     * 
     */
    public LsfileacResponseHolder createLsfileacResponseHolder() {
        return new LsfileacResponseHolder();
    }

    /**
     * Create an instance of {@link LsfileacHostHeader }
     * 
     */
    public LsfileacHostHeader createLsfileacHostHeader() {
        return new LsfileacHostHeader();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileacFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileac", name = "LsfileacFaultInfo")
    public JAXBElement<LsfileacFaultInfo> createLsfileacFaultInfo(LsfileacFaultInfo value) {
        return new JAXBElement<LsfileacFaultInfo>(_LsfileacFaultInfo_QNAME, LsfileacFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileacRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileac", name = "LsfileacRequest")
    public JAXBElement<LsfileacRequest> createLsfileacRequest(LsfileacRequest value) {
        return new JAXBElement<LsfileacRequest>(_LsfileacRequest_QNAME, LsfileacRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileacHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileac", name = "LsfileacHostHeader")
    public JAXBElement<LsfileacHostHeader> createLsfileacHostHeader(LsfileacHostHeader value) {
        return new JAXBElement<LsfileacHostHeader>(_LsfileacHostHeader_QNAME, LsfileacHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileacRequestHolder }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileac", name = "LsfileacRequestHolder")
    public JAXBElement<LsfileacRequestHolder> createLsfileacRequestHolder(LsfileacRequestHolder value) {
        return new JAXBElement<LsfileacRequestHolder>(_LsfileacRequestHolder_QNAME, LsfileacRequestHolder.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileacResponseHolder }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileac", name = "LsfileacResponseHolder")
    public JAXBElement<LsfileacResponseHolder> createLsfileacResponseHolder(LsfileacResponseHolder value) {
        return new JAXBElement<LsfileacResponseHolder>(_LsfileacResponseHolder_QNAME, LsfileacResponseHolder.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileacResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileac", name = "LsfileacResponse")
    public JAXBElement<LsfileacResponse> createLsfileacResponse(LsfileacResponse value) {
        return new JAXBElement<LsfileacResponse>(_LsfileacResponse_QNAME, LsfileacResponse.class, null, value);
    }

}


package com.legstar.test.cixs.lsfileax;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.lsfileax package. 
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

    private final static QName _LsfileacRequest_QNAME = new QName("http://cixs.test.legstar.com/lsfileax", "LsfileacRequest");
    private final static QName _LsfileaeRequest_QNAME = new QName("http://cixs.test.legstar.com/lsfileax", "LsfileaeRequest");
    private final static QName _LsfileaeFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/lsfileax", "LsfileaeFaultInfo");
    private final static QName _LsfileaxHostHeader_QNAME = new QName("http://cixs.test.legstar.com/lsfileax", "LsfileaxHostHeader");
    private final static QName _LsfileacRequestHolder_QNAME = new QName("http://cixs.test.legstar.com/lsfileax", "LsfileacRequestHolder");
    private final static QName _LsfileaeResponse_QNAME = new QName("http://cixs.test.legstar.com/lsfileax", "LsfileaeResponse");
    private final static QName _LsfileacFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/lsfileax", "LsfileacFaultInfo");
    private final static QName _LsfileacResponse_QNAME = new QName("http://cixs.test.legstar.com/lsfileax", "LsfileacResponse");
    private final static QName _LsfileacResponseHolder_QNAME = new QName("http://cixs.test.legstar.com/lsfileax", "LsfileacResponseHolder");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.lsfileax
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link LsfileacFaultInfo }
     * 
     */
    public LsfileacFaultInfo createLsfileacFaultInfo() {
        return new LsfileacFaultInfo();
    }

    /**
     * Create an instance of {@link LsfileaeRequest }
     * 
     */
    public LsfileaeRequest createLsfileaeRequest() {
        return new LsfileaeRequest();
    }

    /**
     * Create an instance of {@link LsfileacResponseHolder }
     * 
     */
    public LsfileacResponseHolder createLsfileacResponseHolder() {
        return new LsfileacResponseHolder();
    }

    /**
     * Create an instance of {@link LsfileaxHostHeader }
     * 
     */
    public LsfileaxHostHeader createLsfileaxHostHeader() {
        return new LsfileaxHostHeader();
    }

    /**
     * Create an instance of {@link LsfileacRequestHolder }
     * 
     */
    public LsfileacRequestHolder createLsfileacRequestHolder() {
        return new LsfileacRequestHolder();
    }

    /**
     * Create an instance of {@link LsfileacRequest }
     * 
     */
    public LsfileacRequest createLsfileacRequest() {
        return new LsfileacRequest();
    }

    /**
     * Create an instance of {@link LsfileaeResponse }
     * 
     */
    public LsfileaeResponse createLsfileaeResponse() {
        return new LsfileaeResponse();
    }

    /**
     * Create an instance of {@link LsfileacResponse }
     * 
     */
    public LsfileacResponse createLsfileacResponse() {
        return new LsfileacResponse();
    }

    /**
     * Create an instance of {@link LsfileaeFaultInfo }
     * 
     */
    public LsfileaeFaultInfo createLsfileaeFaultInfo() {
        return new LsfileaeFaultInfo();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileacRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileax", name = "LsfileacRequest")
    public JAXBElement<LsfileacRequest> createLsfileacRequest(LsfileacRequest value) {
        return new JAXBElement<LsfileacRequest>(_LsfileacRequest_QNAME, LsfileacRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileaeRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileax", name = "LsfileaeRequest")
    public JAXBElement<LsfileaeRequest> createLsfileaeRequest(LsfileaeRequest value) {
        return new JAXBElement<LsfileaeRequest>(_LsfileaeRequest_QNAME, LsfileaeRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileaeFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileax", name = "LsfileaeFaultInfo")
    public JAXBElement<LsfileaeFaultInfo> createLsfileaeFaultInfo(LsfileaeFaultInfo value) {
        return new JAXBElement<LsfileaeFaultInfo>(_LsfileaeFaultInfo_QNAME, LsfileaeFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileaxHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileax", name = "LsfileaxHostHeader")
    public JAXBElement<LsfileaxHostHeader> createLsfileaxHostHeader(LsfileaxHostHeader value) {
        return new JAXBElement<LsfileaxHostHeader>(_LsfileaxHostHeader_QNAME, LsfileaxHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileacRequestHolder }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileax", name = "LsfileacRequestHolder")
    public JAXBElement<LsfileacRequestHolder> createLsfileacRequestHolder(LsfileacRequestHolder value) {
        return new JAXBElement<LsfileacRequestHolder>(_LsfileacRequestHolder_QNAME, LsfileacRequestHolder.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileaeResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileax", name = "LsfileaeResponse")
    public JAXBElement<LsfileaeResponse> createLsfileaeResponse(LsfileaeResponse value) {
        return new JAXBElement<LsfileaeResponse>(_LsfileaeResponse_QNAME, LsfileaeResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileacFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileax", name = "LsfileacFaultInfo")
    public JAXBElement<LsfileacFaultInfo> createLsfileacFaultInfo(LsfileacFaultInfo value) {
        return new JAXBElement<LsfileacFaultInfo>(_LsfileacFaultInfo_QNAME, LsfileacFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileacResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileax", name = "LsfileacResponse")
    public JAXBElement<LsfileacResponse> createLsfileacResponse(LsfileacResponse value) {
        return new JAXBElement<LsfileacResponse>(_LsfileacResponse_QNAME, LsfileacResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfileacResponseHolder }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileax", name = "LsfileacResponseHolder")
    public JAXBElement<LsfileacResponseHolder> createLsfileacResponseHolder(LsfileacResponseHolder value) {
        return new JAXBElement<LsfileacResponseHolder>(_LsfileacResponseHolder_QNAME, LsfileacResponseHolder.class, null, value);
    }

}

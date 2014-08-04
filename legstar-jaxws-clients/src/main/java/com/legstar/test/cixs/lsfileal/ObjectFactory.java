
package com.legstar.test.cixs.lsfileal;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.lsfileal package. 
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

    private final static QName _LsfilealFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/lsfileal", "LsfilealFaultInfo");
    private final static QName _LsfilealHostHeader_QNAME = new QName("http://cixs.test.legstar.com/lsfileal", "LsfilealHostHeader");
    private final static QName _LsfilealResponse_QNAME = new QName("http://cixs.test.legstar.com/lsfileal", "LsfilealResponse");
    private final static QName _LsfilealRequest_QNAME = new QName("http://cixs.test.legstar.com/lsfileal", "LsfilealRequest");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.lsfileal
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link LsfilealHostHeader }
     * 
     */
    public LsfilealHostHeader createLsfilealHostHeader() {
        return new LsfilealHostHeader();
    }

    /**
     * Create an instance of {@link LsfilealFaultInfo }
     * 
     */
    public LsfilealFaultInfo createLsfilealFaultInfo() {
        return new LsfilealFaultInfo();
    }

    /**
     * Create an instance of {@link LsfilealRequest }
     * 
     */
    public LsfilealRequest createLsfilealRequest() {
        return new LsfilealRequest();
    }

    /**
     * Create an instance of {@link LsfilealResponse }
     * 
     */
    public LsfilealResponse createLsfilealResponse() {
        return new LsfilealResponse();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfilealFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileal", name = "LsfilealFaultInfo")
    public JAXBElement<LsfilealFaultInfo> createLsfilealFaultInfo(LsfilealFaultInfo value) {
        return new JAXBElement<LsfilealFaultInfo>(_LsfilealFaultInfo_QNAME, LsfilealFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfilealHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileal", name = "LsfilealHostHeader")
    public JAXBElement<LsfilealHostHeader> createLsfilealHostHeader(LsfilealHostHeader value) {
        return new JAXBElement<LsfilealHostHeader>(_LsfilealHostHeader_QNAME, LsfilealHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfilealResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileal", name = "LsfilealResponse")
    public JAXBElement<LsfilealResponse> createLsfilealResponse(LsfilealResponse value) {
        return new JAXBElement<LsfilealResponse>(_LsfilealResponse_QNAME, LsfilealResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LsfilealRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/lsfileal", name = "LsfilealRequest")
    public JAXBElement<LsfilealRequest> createLsfilealRequest(LsfilealRequest value) {
        return new JAXBElement<LsfilealRequest>(_LsfilealRequest_QNAME, LsfilealRequest.class, null, value);
    }

}

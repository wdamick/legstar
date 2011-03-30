
package com.legstar.test.cixs.osarrays;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.osarrays package. 
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

    private final static QName _OsarraysHostHeader_QNAME = new QName("http://cixs.test.legstar.com/osarrays", "OsarraysHostHeader");
    private final static QName _OsarraysFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/osarrays", "OsarraysFaultInfo");
    private final static QName _OsarraysRequest_QNAME = new QName("http://cixs.test.legstar.com/osarrays", "OsarraysRequest");
    private final static QName _OsarraysResponse_QNAME = new QName("http://cixs.test.legstar.com/osarrays", "OsarraysResponse");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.osarrays
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link OsarraysHostHeader }
     * 
     */
    public OsarraysHostHeader createOsarraysHostHeader() {
        return new OsarraysHostHeader();
    }

    /**
     * Create an instance of {@link OsarraysFaultInfo }
     * 
     */
    public OsarraysFaultInfo createOsarraysFaultInfo() {
        return new OsarraysFaultInfo();
    }

    /**
     * Create an instance of {@link OsarraysResponse }
     * 
     */
    public OsarraysResponse createOsarraysResponse() {
        return new OsarraysResponse();
    }

    /**
     * Create an instance of {@link OsarraysRequest }
     * 
     */
    public OsarraysRequest createOsarraysRequest() {
        return new OsarraysRequest();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link OsarraysHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/osarrays", name = "OsarraysHostHeader")
    public JAXBElement<OsarraysHostHeader> createOsarraysHostHeader(OsarraysHostHeader value) {
        return new JAXBElement<OsarraysHostHeader>(_OsarraysHostHeader_QNAME, OsarraysHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link OsarraysFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/osarrays", name = "OsarraysFaultInfo")
    public JAXBElement<OsarraysFaultInfo> createOsarraysFaultInfo(OsarraysFaultInfo value) {
        return new JAXBElement<OsarraysFaultInfo>(_OsarraysFaultInfo_QNAME, OsarraysFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link OsarraysRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/osarrays", name = "OsarraysRequest")
    public JAXBElement<OsarraysRequest> createOsarraysRequest(OsarraysRequest value) {
        return new JAXBElement<OsarraysRequest>(_OsarraysRequest_QNAME, OsarraysRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link OsarraysResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/osarrays", name = "OsarraysResponse")
    public JAXBElement<OsarraysResponse> createOsarraysResponse(OsarraysResponse value) {
        return new JAXBElement<OsarraysResponse>(_OsarraysResponse_QNAME, OsarraysResponse.class, null, value);
    }

}


package com.legstar.test.cixs.arrayssm;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.arrayssm package. 
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

    private final static QName _ArrayssmRequest_QNAME = new QName("http://cixs.test.legstar.com/arrayssm", "ArrayssmRequest");
    private final static QName _ArrayssmHostHeader_QNAME = new QName("http://cixs.test.legstar.com/arrayssm", "ArrayssmHostHeader");
    private final static QName _ArrayssmFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/arrayssm", "ArrayssmFaultInfo");
    private final static QName _ArrayssmResponse_QNAME = new QName("http://cixs.test.legstar.com/arrayssm", "ArrayssmResponse");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.arrayssm
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link ArrayssmHostHeader }
     * 
     */
    public ArrayssmHostHeader createArrayssmHostHeader() {
        return new ArrayssmHostHeader();
    }

    /**
     * Create an instance of {@link ArrayssmFaultInfo }
     * 
     */
    public ArrayssmFaultInfo createArrayssmFaultInfo() {
        return new ArrayssmFaultInfo();
    }

    /**
     * Create an instance of {@link ArrayssmRequest }
     * 
     */
    public ArrayssmRequest createArrayssmRequest() {
        return new ArrayssmRequest();
    }

    /**
     * Create an instance of {@link ArrayssmResponse }
     * 
     */
    public ArrayssmResponse createArrayssmResponse() {
        return new ArrayssmResponse();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ArrayssmRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/arrayssm", name = "ArrayssmRequest")
    public JAXBElement<ArrayssmRequest> createArrayssmRequest(ArrayssmRequest value) {
        return new JAXBElement<ArrayssmRequest>(_ArrayssmRequest_QNAME, ArrayssmRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ArrayssmHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/arrayssm", name = "ArrayssmHostHeader")
    public JAXBElement<ArrayssmHostHeader> createArrayssmHostHeader(ArrayssmHostHeader value) {
        return new JAXBElement<ArrayssmHostHeader>(_ArrayssmHostHeader_QNAME, ArrayssmHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ArrayssmFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/arrayssm", name = "ArrayssmFaultInfo")
    public JAXBElement<ArrayssmFaultInfo> createArrayssmFaultInfo(ArrayssmFaultInfo value) {
        return new JAXBElement<ArrayssmFaultInfo>(_ArrayssmFaultInfo_QNAME, ArrayssmFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ArrayssmResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/arrayssm", name = "ArrayssmResponse")
    public JAXBElement<ArrayssmResponse> createArrayssmResponse(ArrayssmResponse value) {
        return new JAXBElement<ArrayssmResponse>(_ArrayssmResponse_QNAME, ArrayssmResponse.class, null, value);
    }

}

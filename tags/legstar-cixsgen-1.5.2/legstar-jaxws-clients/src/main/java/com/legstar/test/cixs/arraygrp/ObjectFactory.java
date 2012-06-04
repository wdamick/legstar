
package com.legstar.test.cixs.arraygrp;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.arraygrp package. 
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

    private final static QName _ArraygrpRequest_QNAME = new QName("http://cixs.test.legstar.com/arraygrp", "ArraygrpRequest");
    private final static QName _ArraygrpFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/arraygrp", "ArraygrpFaultInfo");
    private final static QName _ArraygrpHostHeader_QNAME = new QName("http://cixs.test.legstar.com/arraygrp", "ArraygrpHostHeader");
    private final static QName _ArraygrpResponse_QNAME = new QName("http://cixs.test.legstar.com/arraygrp", "ArraygrpResponse");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.arraygrp
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link ArraygrpFaultInfo }
     * 
     */
    public ArraygrpFaultInfo createArraygrpFaultInfo() {
        return new ArraygrpFaultInfo();
    }

    /**
     * Create an instance of {@link ArraygrpHostHeader }
     * 
     */
    public ArraygrpHostHeader createArraygrpHostHeader() {
        return new ArraygrpHostHeader();
    }

    /**
     * Create an instance of {@link ArraygrpRequest }
     * 
     */
    public ArraygrpRequest createArraygrpRequest() {
        return new ArraygrpRequest();
    }

    /**
     * Create an instance of {@link ArraygrpResponse }
     * 
     */
    public ArraygrpResponse createArraygrpResponse() {
        return new ArraygrpResponse();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ArraygrpRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/arraygrp", name = "ArraygrpRequest")
    public JAXBElement<ArraygrpRequest> createArraygrpRequest(ArraygrpRequest value) {
        return new JAXBElement<ArraygrpRequest>(_ArraygrpRequest_QNAME, ArraygrpRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ArraygrpFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/arraygrp", name = "ArraygrpFaultInfo")
    public JAXBElement<ArraygrpFaultInfo> createArraygrpFaultInfo(ArraygrpFaultInfo value) {
        return new JAXBElement<ArraygrpFaultInfo>(_ArraygrpFaultInfo_QNAME, ArraygrpFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ArraygrpHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/arraygrp", name = "ArraygrpHostHeader")
    public JAXBElement<ArraygrpHostHeader> createArraygrpHostHeader(ArraygrpHostHeader value) {
        return new JAXBElement<ArraygrpHostHeader>(_ArraygrpHostHeader_QNAME, ArraygrpHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ArraygrpResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/arraygrp", name = "ArraygrpResponse")
    public JAXBElement<ArraygrpResponse> createArraygrpResponse(ArraygrpResponse value) {
        return new JAXBElement<ArraygrpResponse>(_ArraygrpResponse_QNAME, ArraygrpResponse.class, null, value);
    }

}

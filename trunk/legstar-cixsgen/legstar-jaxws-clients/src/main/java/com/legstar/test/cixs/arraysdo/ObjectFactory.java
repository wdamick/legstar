
package com.legstar.test.cixs.arraysdo;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.arraysdo package. 
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

    private final static QName _ArraysdoRequest_QNAME = new QName("http://cixs.test.legstar.com/arraysdo", "ArraysdoRequest");
    private final static QName _ArraysdoHostHeader_QNAME = new QName("http://cixs.test.legstar.com/arraysdo", "ArraysdoHostHeader");
    private final static QName _ArraysdoFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/arraysdo", "ArraysdoFaultInfo");
    private final static QName _ArraysdoResponse_QNAME = new QName("http://cixs.test.legstar.com/arraysdo", "ArraysdoResponse");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.arraysdo
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link ArraysdoFaultInfo }
     * 
     */
    public ArraysdoFaultInfo createArraysdoFaultInfo() {
        return new ArraysdoFaultInfo();
    }

    /**
     * Create an instance of {@link ArraysdoRequest }
     * 
     */
    public ArraysdoRequest createArraysdoRequest() {
        return new ArraysdoRequest();
    }

    /**
     * Create an instance of {@link ArraysdoHostHeader }
     * 
     */
    public ArraysdoHostHeader createArraysdoHostHeader() {
        return new ArraysdoHostHeader();
    }

    /**
     * Create an instance of {@link ArraysdoResponse }
     * 
     */
    public ArraysdoResponse createArraysdoResponse() {
        return new ArraysdoResponse();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ArraysdoRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/arraysdo", name = "ArraysdoRequest")
    public JAXBElement<ArraysdoRequest> createArraysdoRequest(ArraysdoRequest value) {
        return new JAXBElement<ArraysdoRequest>(_ArraysdoRequest_QNAME, ArraysdoRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ArraysdoHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/arraysdo", name = "ArraysdoHostHeader")
    public JAXBElement<ArraysdoHostHeader> createArraysdoHostHeader(ArraysdoHostHeader value) {
        return new JAXBElement<ArraysdoHostHeader>(_ArraysdoHostHeader_QNAME, ArraysdoHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ArraysdoFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/arraysdo", name = "ArraysdoFaultInfo")
    public JAXBElement<ArraysdoFaultInfo> createArraysdoFaultInfo(ArraysdoFaultInfo value) {
        return new JAXBElement<ArraysdoFaultInfo>(_ArraysdoFaultInfo_QNAME, ArraysdoFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ArraysdoResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/arraysdo", name = "ArraysdoResponse")
    public JAXBElement<ArraysdoResponse> createArraysdoResponse(ArraysdoResponse value) {
        return new JAXBElement<ArraysdoResponse>(_ArraysdoResponse_QNAME, ArraysdoResponse.class, null, value);
    }

}

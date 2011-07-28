
package com.legstar.test.cixs.listssdo;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.listssdo package. 
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

    private final static QName _ListssdoFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/listssdo", "ListssdoFaultInfo");
    private final static QName _ListssdoResponse_QNAME = new QName("http://cixs.test.legstar.com/listssdo", "ListssdoResponse");
    private final static QName _ListssdoHostHeader_QNAME = new QName("http://cixs.test.legstar.com/listssdo", "ListssdoHostHeader");
    private final static QName _ListssdoRequest_QNAME = new QName("http://cixs.test.legstar.com/listssdo", "ListssdoRequest");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.listssdo
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link ListssdoFaultInfo }
     * 
     */
    public ListssdoFaultInfo createListssdoFaultInfo() {
        return new ListssdoFaultInfo();
    }

    /**
     * Create an instance of {@link ListssdoResponse }
     * 
     */
    public ListssdoResponse createListssdoResponse() {
        return new ListssdoResponse();
    }

    /**
     * Create an instance of {@link ListssdoHostHeader }
     * 
     */
    public ListssdoHostHeader createListssdoHostHeader() {
        return new ListssdoHostHeader();
    }

    /**
     * Create an instance of {@link ListssdoRequest }
     * 
     */
    public ListssdoRequest createListssdoRequest() {
        return new ListssdoRequest();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ListssdoFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/listssdo", name = "ListssdoFaultInfo")
    public JAXBElement<ListssdoFaultInfo> createListssdoFaultInfo(ListssdoFaultInfo value) {
        return new JAXBElement<ListssdoFaultInfo>(_ListssdoFaultInfo_QNAME, ListssdoFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ListssdoResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/listssdo", name = "ListssdoResponse")
    public JAXBElement<ListssdoResponse> createListssdoResponse(ListssdoResponse value) {
        return new JAXBElement<ListssdoResponse>(_ListssdoResponse_QNAME, ListssdoResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ListssdoHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/listssdo", name = "ListssdoHostHeader")
    public JAXBElement<ListssdoHostHeader> createListssdoHostHeader(ListssdoHostHeader value) {
        return new JAXBElement<ListssdoHostHeader>(_ListssdoHostHeader_QNAME, ListssdoHostHeader.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ListssdoRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/listssdo", name = "ListssdoRequest")
    public JAXBElement<ListssdoRequest> createListssdoRequest(ListssdoRequest value) {
        return new JAXBElement<ListssdoRequest>(_ListssdoRequest_QNAME, ListssdoRequest.class, null, value);
    }

}


package com.legstar.test.cixs.valuemix;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.cixs.valuemix package. 
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

    private final static QName _ValuemixResponse_QNAME = new QName("http://cixs.test.legstar.com/valuemix", "ValuemixResponse");
    private final static QName _ValuemixFaultInfo_QNAME = new QName("http://cixs.test.legstar.com/valuemix", "ValuemixFaultInfo");
    private final static QName _ValuemixRequest_QNAME = new QName("http://cixs.test.legstar.com/valuemix", "ValuemixRequest");
    private final static QName _ValuemixHostHeader_QNAME = new QName("http://cixs.test.legstar.com/valuemix", "ValuemixHostHeader");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.cixs.valuemix
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link ValuemixHostHeader }
     * 
     */
    public ValuemixHostHeader createValuemixHostHeader() {
        return new ValuemixHostHeader();
    }

    /**
     * Create an instance of {@link ValuemixFaultInfo }
     * 
     */
    public ValuemixFaultInfo createValuemixFaultInfo() {
        return new ValuemixFaultInfo();
    }

    /**
     * Create an instance of {@link ValuemixResponse }
     * 
     */
    public ValuemixResponse createValuemixResponse() {
        return new ValuemixResponse();
    }

    /**
     * Create an instance of {@link ValuemixRequest }
     * 
     */
    public ValuemixRequest createValuemixRequest() {
        return new ValuemixRequest();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ValuemixResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/valuemix", name = "ValuemixResponse")
    public JAXBElement<ValuemixResponse> createValuemixResponse(ValuemixResponse value) {
        return new JAXBElement<ValuemixResponse>(_ValuemixResponse_QNAME, ValuemixResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ValuemixFaultInfo }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/valuemix", name = "ValuemixFaultInfo")
    public JAXBElement<ValuemixFaultInfo> createValuemixFaultInfo(ValuemixFaultInfo value) {
        return new JAXBElement<ValuemixFaultInfo>(_ValuemixFaultInfo_QNAME, ValuemixFaultInfo.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ValuemixRequest }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/valuemix", name = "ValuemixRequest")
    public JAXBElement<ValuemixRequest> createValuemixRequest(ValuemixRequest value) {
        return new JAXBElement<ValuemixRequest>(_ValuemixRequest_QNAME, ValuemixRequest.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ValuemixHostHeader }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://cixs.test.legstar.com/valuemix", name = "ValuemixHostHeader")
    public JAXBElement<ValuemixHostHeader> createValuemixHostHeader(ValuemixHostHeader value) {
        return new JAXBElement<ValuemixHostHeader>(_ValuemixHostHeader_QNAME, ValuemixHostHeader.class, null, value);
    }

}

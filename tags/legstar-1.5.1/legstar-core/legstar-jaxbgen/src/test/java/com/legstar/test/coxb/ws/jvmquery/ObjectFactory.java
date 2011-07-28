
package com.legstar.test.coxb.ws.jvmquery;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.coxb.ws.jvmquery package. 
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

    private final static QName _QueryJvmResponse_QNAME = new QName("http://jvmquery.ws.cases.test.xsdc.legstar.com/", "queryJvmResponse");
    private final static QName _JVMQueryException_QNAME = new QName("http://jvmquery.ws.cases.test.xsdc.legstar.com/", "JVMQueryException");
    private final static QName _QueryJvm_QNAME = new QName("http://jvmquery.ws.cases.test.xsdc.legstar.com/", "queryJvm");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.coxb.ws.jvmquery
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link JVMQueryException }
     * 
     */
    public JVMQueryException createJVMQueryException() {
        return new JVMQueryException();
    }

    /**
     * Create an instance of {@link QueryJvmResponse }
     * 
     */
    public QueryJvmResponse createQueryJvmResponse() {
        return new QueryJvmResponse();
    }

    /**
     * Create an instance of {@link QueryJvm }
     * 
     */
    public QueryJvm createQueryJvm() {
        return new QueryJvm();
    }

    /**
     * Create an instance of {@link JvmQueryReply }
     * 
     */
    public JvmQueryReply createJvmQueryReply() {
        return new JvmQueryReply();
    }

    /**
     * Create an instance of {@link JvmQueryRequest }
     * 
     */
    public JvmQueryRequest createJvmQueryRequest() {
        return new JvmQueryRequest();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link QueryJvmResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://jvmquery.ws.cases.test.xsdc.legstar.com/", name = "queryJvmResponse")
    public JAXBElement<QueryJvmResponse> createQueryJvmResponse(QueryJvmResponse value) {
        return new JAXBElement<QueryJvmResponse>(_QueryJvmResponse_QNAME, QueryJvmResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link JVMQueryException }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://jvmquery.ws.cases.test.xsdc.legstar.com/", name = "JVMQueryException")
    public JAXBElement<JVMQueryException> createJVMQueryException(JVMQueryException value) {
        return new JAXBElement<JVMQueryException>(_JVMQueryException_QNAME, JVMQueryException.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link QueryJvm }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://jvmquery.ws.cases.test.xsdc.legstar.com/", name = "queryJvm")
    public JAXBElement<QueryJvm> createQueryJvm(QueryJvm value) {
        return new JAXBElement<QueryJvm>(_QueryJvm_QNAME, QueryJvm.class, null, value);
    }

}

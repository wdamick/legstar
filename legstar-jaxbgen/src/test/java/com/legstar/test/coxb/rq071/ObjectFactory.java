
package com.legstar.test.coxb.rq071;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.coxb.rq071 package. 
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

    private final static QName _RQ071OutputPart_QNAME = new QName("http://creditstatus.customer.ibg/", "RQ071OutputPart");
    private final static QName _RQ071Input_QNAME = new QName("http://creditstatus.customer.ibg/", "rQ071Input");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.coxb.rq071
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link RQ071Output }
     * 
     */
    public RQ071Output createRQ071Output() {
        return new RQ071Output();
    }

    /**
     * Create an instance of {@link RQ071Input }
     * 
     */
    public RQ071Input createRQ071Input() {
        return new RQ071Input();
    }

    /**
     * Create an instance of {@link Eb017output_rq071__check }
     * 
     */
    public Eb017output_rq071__check createEb017output_rq071__check() {
        return new Eb017output_rq071__check();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RQ071Output }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://creditstatus.customer.ibg/", name = "RQ071OutputPart")
    public JAXBElement<RQ071Output> createRQ071OutputPart(RQ071Output value) {
        return new JAXBElement<RQ071Output>(_RQ071OutputPart_QNAME, RQ071Output.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RQ071Input }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://creditstatus.customer.ibg/", name = "rQ071Input")
    public JAXBElement<RQ071Input> createRQ071Input(RQ071Input value) {
        return new JAXBElement<RQ071Input>(_RQ071Input_QNAME, RQ071Input.class, null, value);
    }

}

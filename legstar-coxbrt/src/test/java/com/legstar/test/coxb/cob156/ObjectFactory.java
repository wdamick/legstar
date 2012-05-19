package com.legstar.test.coxb.cob156;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;

/**
 * This object contains factory methods for each Java content interface and Java
 * element interface generated in the legstar.trans.naor5 package.
 * <p>
 * An ObjectFactory allows you to programatically construct new instances of the
 * Java representation for XML content. The Java representation of XML content
 * can consist of schema derived interfaces and classes representing the binding
 * of schema type definitions, element declarations and model groups. Factory
 * methods for each of these are provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {

    private final static QName _A_QNAME = new QName(
            "http://com.legstar.test.coxb/cob156", "a");

    /**
     * Create a new ObjectFactory that can be used to create new instances of
     * schema derived classes for package: legstar.trans.naor5
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link C }
     * 
     */
    public C createC() {
        return new C();
    }

    /**
     * Create an instance of {@link A }
     * 
     */
    public A createA() {
        return new A();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link A }{@code >}
     * 
     */
    @XmlElementDecl(namespace = "http://com.legstar.test.coxb/cob156", name = "a")
    public JAXBElement < A > createA(A value) {
        return new JAXBElement < A >(_A_QNAME, A.class, null, value);
    }

}

package com.legstar.test.coxb.pojo156.jaxb;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;

/**
 * This object contains factory methods for each Java content interface and Java
 * element interface generated in the legstar.trans.naor3 package.
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

    private final static QName _SomeClass_QNAME = new QName(
            "http://com.legstar.test.coxb/pojo156", "someClass");

    /**
     * Create a new ObjectFactory that can be used to create new instances of
     * schema derived classes for package: legstar.trans.naor3
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link SomeClass }
     * 
     */
    public SomeClass createSomeClass() {
        return new SomeClass();
    }

    /**
     * Create an instance of {@link SomeItem }
     * 
     */
    public SomeItem createSomeItem() {
        return new SomeItem();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link SomeClass }
     * {@code >}
     * 
     */
    @XmlElementDecl(namespace = "http://com.legstar.test.coxb/pojo156", name = "someClass")
    public JAXBElement < SomeClass > createSomeClass(SomeClass value) {
        return new JAXBElement < SomeClass >(_SomeClass_QNAME, SomeClass.class,
                null, value);
    }

}

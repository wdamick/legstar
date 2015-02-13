
package com.legstar.test.coxb.ardo02;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.coxb.ardo02 package. 
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

    private final static QName _Ardo02Record_QNAME = new QName("http://legstar.com/test/coxb/ardo02", "Ardo02Record");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.coxb.ardo02
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link AlternativeB }
     * 
     */
    public AlternativeB createAlternativeB() {
        return new AlternativeB();
    }

    /**
     * Create an instance of {@link OdoArray }
     * 
     */
    public OdoArray createOdoArray() {
        return new OdoArray();
    }

    /**
     * Create an instance of {@link Ardo02Record }
     * 
     */
    public Ardo02Record createArdo02Record() {
        return new Ardo02Record();
    }

    /**
     * Create an instance of {@link AlternativeA }
     * 
     */
    public AlternativeA createAlternativeA() {
        return new AlternativeA();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Ardo02Record }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/ardo02", name = "Ardo02Record")
    public JAXBElement<Ardo02Record> createArdo02Record(Ardo02Record value) {
        return new JAXBElement<Ardo02Record>(_Ardo02Record_QNAME, Ardo02Record.class, null, value);
    }

}


package com.legstar.test.coxb.arrayscx;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.coxb.arrayscx package. 
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

    private final static QName _Dfhcommarea_QNAME = new QName("http://legstar.com/test/coxb/arrayscx", "Dfhcommarea");
    private final static QName _Filler23_QNAME = new QName("http://legstar.com/test/coxb/arrayscx", "Filler23");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.coxb.arrayscx
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link ElementOne }
     * 
     */
    public ElementOne createElementOne() {
        return new ElementOne();
    }

    /**
     * Create an instance of {@link TableRedef }
     * 
     */
    public TableRedef createTableRedef() {
        return new TableRedef();
    }

    /**
     * Create an instance of {@link ElementTwo }
     * 
     */
    public ElementTwo createElementTwo() {
        return new ElementTwo();
    }

    /**
     * Create an instance of {@link Filler23 }
     * 
     */
    public Filler23 createFiller23() {
        return new Filler23();
    }

    /**
     * Create an instance of {@link Dfhcommarea }
     * 
     */
    public Dfhcommarea createDfhcommarea() {
        return new Dfhcommarea();
    }

    /**
     * Create an instance of {@link TableThree }
     * 
     */
    public TableThree createTableThree() {
        return new TableThree();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Dfhcommarea }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/arrayscx", name = "Dfhcommarea")
    public JAXBElement<Dfhcommarea> createDfhcommarea(Dfhcommarea value) {
        return new JAXBElement<Dfhcommarea>(_Dfhcommarea_QNAME, Dfhcommarea.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Filler23 }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/arrayscx", name = "Filler23")
    public JAXBElement<Filler23> createFiller23(Filler23 value) {
        return new JAXBElement<Filler23>(_Filler23_QNAME, Filler23 .class, null, value);
    }

}

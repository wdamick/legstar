
package com.legstar.test.coxb.issue162;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.coxb.issue162 package. 
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

    private final static QName _Dfhcommarea_QNAME = new QName("http://coxb.test.legstar.com/issue162", "dfhcommarea");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.coxb.issue162
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link ConfprtyFrmtq }
     * 
     */
    public ConfprtyFrmtq createConfprtyFrmtq() {
        return new ConfprtyFrmtq();
    }

    /**
     * Create an instance of {@link Dfhcommarea }
     * 
     */
    public Dfhcommarea createDfhcommarea() {
        return new Dfhcommarea();
    }

    /**
     * Create an instance of {@link ConfprtyFrmtr }
     * 
     */
    public ConfprtyFrmtr createConfprtyFrmtr() {
        return new ConfprtyFrmtr();
    }

    /**
     * Create an instance of {@link Alteprty }
     * 
     */
    public Alteprty createAlteprty() {
        return new Alteprty();
    }

    /**
     * Create an instance of {@link Confprty }
     * 
     */
    public Confprty createConfprty() {
        return new Confprty();
    }

    /**
     * Create an instance of {@link ConfprtyFrmtp }
     * 
     */
    public ConfprtyFrmtp createConfprtyFrmtp() {
        return new ConfprtyFrmtp();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Dfhcommarea }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://coxb.test.legstar.com/issue162", name = "dfhcommarea")
    public JAXBElement<Dfhcommarea> createDfhcommarea(Dfhcommarea value) {
        return new JAXBElement<Dfhcommarea>(_Dfhcommarea_QNAME, Dfhcommarea.class, null, value);
    }

}


package com.legstar.test.coxb.issue185;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.coxb.issue185 package. 
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

    private final static QName _Commarea_QNAME = new QName("http://coxb.test.legstar.com/issue185", "commarea");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.coxb.issue185
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link Commarea }
     * 
     */
    public Commarea createCommarea() {
        return new Commarea();
    }

    /**
     * Create an instance of {@link OuterRedefinesShort }
     * 
     */
    public OuterRedefinesShort createOuterRedefinesShort() {
        return new OuterRedefinesShort();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Commarea }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://coxb.test.legstar.com/issue185", name = "commarea")
    public JAXBElement<Commarea> createCommarea(Commarea value) {
        return new JAXBElement<Commarea>(_Commarea_QNAME, Commarea.class, null, value);
    }

}

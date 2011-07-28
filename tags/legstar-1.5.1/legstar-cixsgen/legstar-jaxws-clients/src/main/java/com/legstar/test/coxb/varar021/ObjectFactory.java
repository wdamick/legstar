
package com.legstar.test.coxb.varar021;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.coxb.varar021 package. 
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

    private final static QName _SearchGrplst_QNAME = new QName("http://legstar.com/test/coxb/varar021", "SearchGrplst");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.coxb.varar021
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link WellpointEaiEbsErrorRow }
     * 
     */
    public WellpointEaiEbsErrorRow createWellpointEaiEbsErrorRow() {
        return new WellpointEaiEbsErrorRow();
    }

    /**
     * Create an instance of {@link IStaticData }
     * 
     */
    public IStaticData createIStaticData() {
        return new IStaticData();
    }

    /**
     * Create an instance of {@link ODynamicData }
     * 
     */
    public ODynamicData createODynamicData() {
        return new ODynamicData();
    }

    /**
     * Create an instance of {@link LkupInfo39 }
     * 
     */
    public LkupInfo39 createLkupInfo39() {
        return new LkupInfo39();
    }

    /**
     * Create an instance of {@link LkupInfo44 }
     * 
     */
    public LkupInfo44 createLkupInfo44() {
        return new LkupInfo44();
    }

    /**
     * Create an instance of {@link Payload }
     * 
     */
    public Payload createPayload() {
        return new Payload();
    }

    /**
     * Create an instance of {@link SearchGrplst }
     * 
     */
    public SearchGrplst createSearchGrplst() {
        return new SearchGrplst();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link SearchGrplst }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/varar021", name = "SearchGrplst")
    public JAXBElement<SearchGrplst> createSearchGrplst(SearchGrplst value) {
        return new JAXBElement<SearchGrplst>(_SearchGrplst_QNAME, SearchGrplst.class, null, value);
    }

}


package com.legstar.test.coxb.lsfileal;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.coxb.lsfileal package. 
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

    private final static QName _RequestParms_QNAME = new QName("http://legstar.com/test/coxb/lsfileal", "RequestParms");
    private final static QName _WNameMatch_QNAME = new QName("http://legstar.com/test/coxb/lsfileal", "WNameMatch");
    private final static QName _WVisitMode_QNAME = new QName("http://legstar.com/test/coxb/lsfileal", "WVisitMode");
    private final static QName _ReplyData_QNAME = new QName("http://legstar.com/test/coxb/lsfileal", "ReplyData");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.coxb.lsfileal
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link ReplyData }
     * 
     */
    public ReplyData createReplyData() {
        return new ReplyData();
    }

    /**
     * Create an instance of {@link Filler65 }
     * 
     */
    public Filler65 createFiller65() {
        return new Filler65();
    }

    /**
     * Create an instance of {@link ReplyItem }
     * 
     */
    public ReplyItem createReplyItem() {
        return new ReplyItem();
    }

    /**
     * Create an instance of {@link ReplySuccessHeader }
     * 
     */
    public ReplySuccessHeader createReplySuccessHeader() {
        return new ReplySuccessHeader();
    }

    /**
     * Create an instance of {@link ReplyPersonal }
     * 
     */
    public ReplyPersonal createReplyPersonal() {
        return new ReplyPersonal();
    }

    /**
     * Create an instance of {@link RequestParms }
     * 
     */
    public RequestParms createRequestParms() {
        return new RequestParms();
    }

    /**
     * Create an instance of {@link ReplyErrorHeader }
     * 
     */
    public ReplyErrorHeader createReplyErrorHeader() {
        return new ReplyErrorHeader();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RequestParms }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/lsfileal", name = "RequestParms")
    public JAXBElement<RequestParms> createRequestParms(RequestParms value) {
        return new JAXBElement<RequestParms>(_RequestParms_QNAME, RequestParms.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Integer }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/lsfileal", name = "WNameMatch")
    public JAXBElement<Integer> createWNameMatch(Integer value) {
        return new JAXBElement<Integer>(_WNameMatch_QNAME, Integer.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Integer }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/lsfileal", name = "WVisitMode")
    public JAXBElement<Integer> createWVisitMode(Integer value) {
        return new JAXBElement<Integer>(_WVisitMode_QNAME, Integer.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ReplyData }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/lsfileal", name = "ReplyData")
    public JAXBElement<ReplyData> createReplyData(ReplyData value) {
        return new JAXBElement<ReplyData>(_ReplyData_QNAME, ReplyData.class, null, value);
    }

}


package com.legstar.test.coxb.lsfileac;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.coxb.lsfileac package. 
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

    private final static QName _ReplyData_QNAME = new QName("http://legstar.com/test/coxb/lsfileac", "ReplyData");
    private final static QName _QueryLimit_QNAME = new QName("http://legstar.com/test/coxb/lsfileac", "QueryLimit");
    private final static QName _ReplyStatus_QNAME = new QName("http://legstar.com/test/coxb/lsfileac", "ReplyStatus");
    private final static QName _QueryData_QNAME = new QName("http://legstar.com/test/coxb/lsfileac", "QueryData");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.coxb.lsfileac
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link ReplyPersonal }
     * 
     */
    public ReplyPersonal createReplyPersonal() {
        return new ReplyPersonal();
    }

    /**
     * Create an instance of {@link ReplyItem }
     * 
     */
    public ReplyItem createReplyItem() {
        return new ReplyItem();
    }

    /**
     * Create an instance of {@link ReplyStatus }
     * 
     */
    public ReplyStatus createReplyStatus() {
        return new ReplyStatus();
    }

    /**
     * Create an instance of {@link ReplyData }
     * 
     */
    public ReplyData createReplyData() {
        return new ReplyData();
    }

    /**
     * Create an instance of {@link QueryLimit }
     * 
     */
    public QueryLimit createQueryLimit() {
        return new QueryLimit();
    }

    /**
     * Create an instance of {@link QueryData }
     * 
     */
    public QueryData createQueryData() {
        return new QueryData();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ReplyData }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/lsfileac", name = "ReplyData")
    public JAXBElement<ReplyData> createReplyData(ReplyData value) {
        return new JAXBElement<ReplyData>(_ReplyData_QNAME, ReplyData.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link QueryLimit }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/lsfileac", name = "QueryLimit")
    public JAXBElement<QueryLimit> createQueryLimit(QueryLimit value) {
        return new JAXBElement<QueryLimit>(_QueryLimit_QNAME, QueryLimit.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ReplyStatus }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/lsfileac", name = "ReplyStatus")
    public JAXBElement<ReplyStatus> createReplyStatus(ReplyStatus value) {
        return new JAXBElement<ReplyStatus>(_ReplyStatus_QNAME, ReplyStatus.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link QueryData }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/lsfileac", name = "QueryData")
    public JAXBElement<QueryData> createQueryData(QueryData value) {
        return new JAXBElement<QueryData>(_QueryData_QNAME, QueryData.class, null, value);
    }

}


package com.legstar.test.coxb.tcobwvb;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.legstar.test.coxb.tcobwvb package. 
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

    private final static QName _WLastNames_QNAME = new QName("http://legstar.com/test/coxb/tcobwvb", "WLastNames");
    private final static QName _WAddresses_QNAME = new QName("http://legstar.com/test/coxb/tcobwvb", "WAddresses");
    private final static QName _Filler79_QNAME = new QName("http://legstar.com/test/coxb/tcobwvb", "Filler79");
    private final static QName _WPhones_QNAME = new QName("http://legstar.com/test/coxb/tcobwvb", "WPhones");
    private final static QName _Filler95_QNAME = new QName("http://legstar.com/test/coxb/tcobwvb", "Filler95");
    private final static QName _CustomerData_QNAME = new QName("http://legstar.com/test/coxb/tcobwvb", "CustomerData");
    private final static QName _Filler63_QNAME = new QName("http://legstar.com/test/coxb/tcobwvb", "Filler63");
    private final static QName _Filler71_QNAME = new QName("http://legstar.com/test/coxb/tcobwvb", "Filler71");
    private final static QName _WDates_QNAME = new QName("http://legstar.com/test/coxb/tcobwvb", "WDates");
    private final static QName _Filler87_QNAME = new QName("http://legstar.com/test/coxb/tcobwvb", "Filler87");
    private final static QName _WFirstNames_QNAME = new QName("http://legstar.com/test/coxb/tcobwvb", "WFirstNames");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.legstar.test.coxb.tcobwvb
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link Filler63 }
     * 
     */
    public Filler63 createFiller63() {
        return new Filler63();
    }

    /**
     * Create an instance of {@link Filler40 }
     * 
     */
    public Filler40 createFiller40() {
        return new Filler40();
    }

    /**
     * Create an instance of {@link WPhones }
     * 
     */
    public WPhones createWPhones() {
        return new WPhones();
    }

    /**
     * Create an instance of {@link WLastNames }
     * 
     */
    public WLastNames createWLastNames() {
        return new WLastNames();
    }

    /**
     * Create an instance of {@link WFirstNames }
     * 
     */
    public WFirstNames createWFirstNames() {
        return new WFirstNames();
    }

    /**
     * Create an instance of {@link WAddresses }
     * 
     */
    public WAddresses createWAddresses() {
        return new WAddresses();
    }

    /**
     * Create an instance of {@link Transaction }
     * 
     */
    public Transaction createTransaction() {
        return new Transaction();
    }

    /**
     * Create an instance of {@link Filler71 }
     * 
     */
    public Filler71 createFiller71() {
        return new Filler71();
    }

    /**
     * Create an instance of {@link Filler95 }
     * 
     */
    public Filler95 createFiller95() {
        return new Filler95();
    }

    /**
     * Create an instance of {@link Filler87 }
     * 
     */
    public Filler87 createFiller87() {
        return new Filler87();
    }

    /**
     * Create an instance of {@link WDates }
     * 
     */
    public WDates createWDates() {
        return new WDates();
    }

    /**
     * Create an instance of {@link PersonalData }
     * 
     */
    public PersonalData createPersonalData() {
        return new PersonalData();
    }

    /**
     * Create an instance of {@link Filler79 }
     * 
     */
    public Filler79 createFiller79() {
        return new Filler79();
    }

    /**
     * Create an instance of {@link Transactions }
     * 
     */
    public Transactions createTransactions() {
        return new Transactions();
    }

    /**
     * Create an instance of {@link CustomerData }
     * 
     */
    public CustomerData createCustomerData() {
        return new CustomerData();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link WLastNames }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/tcobwvb", name = "WLastNames")
    public JAXBElement<WLastNames> createWLastNames(WLastNames value) {
        return new JAXBElement<WLastNames>(_WLastNames_QNAME, WLastNames.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link WAddresses }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/tcobwvb", name = "WAddresses")
    public JAXBElement<WAddresses> createWAddresses(WAddresses value) {
        return new JAXBElement<WAddresses>(_WAddresses_QNAME, WAddresses.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Filler79 }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/tcobwvb", name = "Filler79")
    public JAXBElement<Filler79> createFiller79(Filler79 value) {
        return new JAXBElement<Filler79>(_Filler79_QNAME, Filler79 .class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link WPhones }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/tcobwvb", name = "WPhones")
    public JAXBElement<WPhones> createWPhones(WPhones value) {
        return new JAXBElement<WPhones>(_WPhones_QNAME, WPhones.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Filler95 }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/tcobwvb", name = "Filler95")
    public JAXBElement<Filler95> createFiller95(Filler95 value) {
        return new JAXBElement<Filler95>(_Filler95_QNAME, Filler95 .class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link CustomerData }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/tcobwvb", name = "CustomerData")
    public JAXBElement<CustomerData> createCustomerData(CustomerData value) {
        return new JAXBElement<CustomerData>(_CustomerData_QNAME, CustomerData.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Filler63 }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/tcobwvb", name = "Filler63")
    public JAXBElement<Filler63> createFiller63(Filler63 value) {
        return new JAXBElement<Filler63>(_Filler63_QNAME, Filler63 .class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Filler71 }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/tcobwvb", name = "Filler71")
    public JAXBElement<Filler71> createFiller71(Filler71 value) {
        return new JAXBElement<Filler71>(_Filler71_QNAME, Filler71 .class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link WDates }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/tcobwvb", name = "WDates")
    public JAXBElement<WDates> createWDates(WDates value) {
        return new JAXBElement<WDates>(_WDates_QNAME, WDates.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Filler87 }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/tcobwvb", name = "Filler87")
    public JAXBElement<Filler87> createFiller87(Filler87 value) {
        return new JAXBElement<Filler87>(_Filler87_QNAME, Filler87 .class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link WFirstNames }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://legstar.com/test/coxb/tcobwvb", name = "WFirstNames")
    public JAXBElement<WFirstNames> createWFirstNames(WFirstNames value) {
        return new JAXBElement<WFirstNames>(_WFirstNames_QNAME, WFirstNames.class, null, value);
    }

}

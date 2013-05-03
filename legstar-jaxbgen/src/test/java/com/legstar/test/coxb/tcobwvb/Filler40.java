
package com.legstar.test.coxb.tcobwvb;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for Filler40 complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Filler40">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="TransactionDay">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="2"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler42">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="1"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="TransactionMonth">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="2"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler44">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="1"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="TransactionYear">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="2"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Filler40", propOrder = {
    "transactionDay",
    "filler42",
    "transactionMonth",
    "filler44",
    "transactionYear"
})
public class Filler40
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "TransactionDay", required = true)
    @CobolElement(cobolName = "TRANSACTION-DAY", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 20, picture = "X(2)", srceLine = 41)
    protected String transactionDay;
    @XmlElement(name = "Filler42", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 20, picture = "X", srceLine = 42)
    protected String filler42;
    @XmlElement(name = "TransactionMonth", required = true)
    @CobolElement(cobolName = "TRANSACTION-MONTH", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 20, picture = "X(2)", srceLine = 43)
    protected String transactionMonth;
    @XmlElement(name = "Filler44", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 20, picture = "X", srceLine = 44)
    protected String filler44;
    @XmlElement(name = "TransactionYear", required = true)
    @CobolElement(cobolName = "TRANSACTION-YEAR", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 20, picture = "X(2)", srceLine = 45)
    protected String transactionYear;

    /**
     * Gets the value of the transactionDay property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTransactionDay() {
        return transactionDay;
    }

    /**
     * Sets the value of the transactionDay property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTransactionDay(String value) {
        this.transactionDay = value;
    }

    public boolean isSetTransactionDay() {
        return (this.transactionDay!= null);
    }

    /**
     * Gets the value of the filler42 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller42() {
        return filler42;
    }

    /**
     * Sets the value of the filler42 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller42(String value) {
        this.filler42 = value;
    }

    public boolean isSetFiller42() {
        return (this.filler42 != null);
    }

    /**
     * Gets the value of the transactionMonth property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTransactionMonth() {
        return transactionMonth;
    }

    /**
     * Sets the value of the transactionMonth property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTransactionMonth(String value) {
        this.transactionMonth = value;
    }

    public boolean isSetTransactionMonth() {
        return (this.transactionMonth!= null);
    }

    /**
     * Gets the value of the filler44 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller44() {
        return filler44;
    }

    /**
     * Sets the value of the filler44 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller44(String value) {
        this.filler44 = value;
    }

    public boolean isSetFiller44() {
        return (this.filler44 != null);
    }

    /**
     * Gets the value of the transactionYear property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTransactionYear() {
        return transactionYear;
    }

    /**
     * Sets the value of the transactionYear property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTransactionYear(String value) {
        this.transactionYear = value;
    }

    public boolean isSetTransactionYear() {
        return (this.transactionYear!= null);
    }

}

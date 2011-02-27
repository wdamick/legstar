
package com.legstar.test.coxb.lsfileaq;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for Customer complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Customer">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CustomerId">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *               &lt;totalDigits value="6"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="PersonalData" type="{http://legstar.com/test/coxb/lsfileaq}PersonalData"/>
 *         &lt;choice>
 *           &lt;element name="LastTransDate">
 *             &lt;simpleType>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *                 &lt;maxLength value="8"/>
 *               &lt;/restriction>
 *             &lt;/simpleType>
 *           &lt;/element>
 *           &lt;element name="Filler49" type="{http://legstar.com/test/coxb/lsfileaq}Filler49"/>
 *         &lt;/choice>
 *         &lt;element name="LastTransAmount">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="8"/>
 *               &lt;pattern value="($|\d|\s)?\d{0,4}\.?\d{0,2}"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="LastTransComment">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="9"/>
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
@XmlType(name = "Customer", propOrder = {
    "customerId",
    "personalData",
    "lastTransDate",
    "filler49",
    "lastTransAmount",
    "lastTransComment"
})
public class Customer
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "CustomerId")
    @CobolElement(cobolName = "CUSTOMER-ID", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 15, isSigned = false, totalDigits = 6, picture = "9(6)", srceLine = 43)
    protected long customerId;
    @XmlElement(name = "PersonalData", required = true)
    @CobolElement(cobolName = "PERSONAL-DATA", type = CobolType.GROUP_ITEM, levelNumber = 15, srceLine = 44)
    protected PersonalData personalData;
    @XmlElement(name = "LastTransDate")
    @CobolElement(cobolName = "LAST-TRANS-DATE", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 15, isRedefined = true, picture = "X(8)", srceLine = 48)
    protected String lastTransDate;
    @XmlElement(name = "Filler49")
    @CobolElement(cobolName = "FILLER", type = CobolType.GROUP_ITEM, levelNumber = 15, redefines = "LAST-TRANS-DATE", srceLine = 49)
    protected Filler49 filler49;
    @XmlElement(name = "LastTransAmount", required = true)
    @CobolElement(cobolName = "LAST-TRANS-AMOUNT", type = CobolType.NUMERIC_EDITED_ITEM, levelNumber = 15, isSigned = false, totalDigits = 6, fractionDigits = 2, picture = "$9999.99", srceLine = 55)
    protected String lastTransAmount;
    @XmlElement(name = "LastTransComment", required = true)
    @CobolElement(cobolName = "LAST-TRANS-COMMENT", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 15, picture = "X(9)", srceLine = 56)
    protected String lastTransComment;

    /**
     * Gets the value of the customerId property.
     * 
     */
    public long getCustomerId() {
        return customerId;
    }

    /**
     * Sets the value of the customerId property.
     * 
     */
    public void setCustomerId(long value) {
        this.customerId = value;
    }

    public boolean isSetCustomerId() {
        return true;
    }

    /**
     * Gets the value of the personalData property.
     * 
     * @return
     *     possible object is
     *     {@link PersonalData }
     *     
     */
    public PersonalData getPersonalData() {
        return personalData;
    }

    /**
     * Sets the value of the personalData property.
     * 
     * @param value
     *     allowed object is
     *     {@link PersonalData }
     *     
     */
    public void setPersonalData(PersonalData value) {
        this.personalData = value;
    }

    public boolean isSetPersonalData() {
        return (this.personalData!= null);
    }

    /**
     * Gets the value of the lastTransDate property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLastTransDate() {
        return lastTransDate;
    }

    /**
     * Sets the value of the lastTransDate property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLastTransDate(String value) {
        this.lastTransDate = value;
    }

    public boolean isSetLastTransDate() {
        return (this.lastTransDate!= null);
    }

    /**
     * Gets the value of the filler49 property.
     * 
     * @return
     *     possible object is
     *     {@link Filler49 }
     *     
     */
    public Filler49 getFiller49() {
        return filler49;
    }

    /**
     * Sets the value of the filler49 property.
     * 
     * @param value
     *     allowed object is
     *     {@link Filler49 }
     *     
     */
    public void setFiller49(Filler49 value) {
        this.filler49 = value;
    }

    public boolean isSetFiller49() {
        return (this.filler49 != null);
    }

    /**
     * Gets the value of the lastTransAmount property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLastTransAmount() {
        return lastTransAmount;
    }

    /**
     * Sets the value of the lastTransAmount property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLastTransAmount(String value) {
        this.lastTransAmount = value;
    }

    public boolean isSetLastTransAmount() {
        return (this.lastTransAmount!= null);
    }

    /**
     * Gets the value of the lastTransComment property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLastTransComment() {
        return lastTransComment;
    }

    /**
     * Sets the value of the lastTransComment property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLastTransComment(String value) {
        this.lastTransComment = value;
    }

    public boolean isSetLastTransComment() {
        return (this.lastTransComment!= null);
    }

}

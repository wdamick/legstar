
package com.legstar.test.coxb.cultureinfo;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for cultureInfoReply complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="cultureInfoReply">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="currencySymbol" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="displayCountry" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="displayLanguage" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="formattedDate" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="formattedDecimalNumber" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="serverCultureInfo" type="{http://cultureinfo.cases.test.xsdc.legstar.com/}serverCultureInfo" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "cultureInfoReply", propOrder = {
    "currencySymbol",
    "displayCountry",
    "displayLanguage",
    "formattedDate",
    "formattedDecimalNumber",
    "serverCultureInfo"
})
public class CultureInfoReply
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @CobolElement(cobolName = "currencySymbol", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(32)", usage = "DISPLAY")
    protected String currencySymbol;
    @CobolElement(cobolName = "displayCountry", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(32)", usage = "DISPLAY")
    protected String displayCountry;
    @CobolElement(cobolName = "displayLanguage", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(32)", usage = "DISPLAY")
    protected String displayLanguage;
    @CobolElement(cobolName = "formattedDate", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(32)", usage = "DISPLAY")
    protected String formattedDate;
    @CobolElement(cobolName = "formattedDecimalNumber", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(32)", usage = "DISPLAY")
    protected String formattedDecimalNumber;
    @CobolElement(cobolName = "serverCultureInfo", type = CobolType.GROUP_ITEM, levelNumber = 5)
    protected ServerCultureInfo serverCultureInfo;

    /**
     * Gets the value of the currencySymbol property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCurrencySymbol() {
        return currencySymbol;
    }

    /**
     * Sets the value of the currencySymbol property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCurrencySymbol(String value) {
        this.currencySymbol = value;
    }

    public boolean isSetCurrencySymbol() {
        return (this.currencySymbol!= null);
    }

    /**
     * Gets the value of the displayCountry property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDisplayCountry() {
        return displayCountry;
    }

    /**
     * Sets the value of the displayCountry property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDisplayCountry(String value) {
        this.displayCountry = value;
    }

    public boolean isSetDisplayCountry() {
        return (this.displayCountry!= null);
    }

    /**
     * Gets the value of the displayLanguage property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDisplayLanguage() {
        return displayLanguage;
    }

    /**
     * Sets the value of the displayLanguage property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDisplayLanguage(String value) {
        this.displayLanguage = value;
    }

    public boolean isSetDisplayLanguage() {
        return (this.displayLanguage!= null);
    }

    /**
     * Gets the value of the formattedDate property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFormattedDate() {
        return formattedDate;
    }

    /**
     * Sets the value of the formattedDate property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFormattedDate(String value) {
        this.formattedDate = value;
    }

    public boolean isSetFormattedDate() {
        return (this.formattedDate!= null);
    }

    /**
     * Gets the value of the formattedDecimalNumber property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFormattedDecimalNumber() {
        return formattedDecimalNumber;
    }

    /**
     * Sets the value of the formattedDecimalNumber property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFormattedDecimalNumber(String value) {
        this.formattedDecimalNumber = value;
    }

    public boolean isSetFormattedDecimalNumber() {
        return (this.formattedDecimalNumber!= null);
    }

    /**
     * Gets the value of the serverCultureInfo property.
     * 
     * @return
     *     possible object is
     *     {@link ServerCultureInfo }
     *     
     */
    public ServerCultureInfo getServerCultureInfo() {
        return serverCultureInfo;
    }

    /**
     * Sets the value of the serverCultureInfo property.
     * 
     * @param value
     *     allowed object is
     *     {@link ServerCultureInfo }
     *     
     */
    public void setServerCultureInfo(ServerCultureInfo value) {
        this.serverCultureInfo = value;
    }

    public boolean isSetServerCultureInfo() {
        return (this.serverCultureInfo!= null);
    }

}

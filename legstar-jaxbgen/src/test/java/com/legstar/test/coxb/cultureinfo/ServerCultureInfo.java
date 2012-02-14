
package com.legstar.test.coxb.cultureinfo;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for serverCultureInfo complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="serverCultureInfo">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="cultureCode" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="displayCountry" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="displayLanguage" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "serverCultureInfo", propOrder = {
    "cultureCode",
    "displayCountry",
    "displayLanguage"
})
public class ServerCultureInfo
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @CobolElement(cobolName = "cultureCode", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 7, picture = "X(32)", usage = "DISPLAY")
    protected String cultureCode;
    @CobolElement(cobolName = "displayCountry", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 7, picture = "X(32)", usage = "DISPLAY")
    protected String displayCountry;
    @CobolElement(cobolName = "displayLanguage", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 7, picture = "X(32)", usage = "DISPLAY")
    protected String displayLanguage;

    /**
     * Gets the value of the cultureCode property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCultureCode() {
        return cultureCode;
    }

    /**
     * Sets the value of the cultureCode property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCultureCode(String value) {
        this.cultureCode = value;
    }

    public boolean isSetCultureCode() {
        return (this.cultureCode!= null);
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

}

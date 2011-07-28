
package com.legstar.test.coxb.jvmquery;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolComplexType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for jvmQueryReply complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="jvmQueryReply">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="country" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="currencySymbol" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="envVarValues" type="{http://www.w3.org/2001/XMLSchema}string" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="formattedDate" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="language" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "jvmQueryReply", propOrder = {
    "country",
    "currencySymbol",
    "envVarValues",
    "formattedDate",
    "language"
})
@CobolComplexType(javaClassName = "com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply")
public class JvmQueryReply
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @CobolElement(cobolName = "country", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 3, picture = "X(32)", usage = "DISPLAY")
    protected String country;
    @CobolElement(cobolName = "currencySymbol", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 3, picture = "X(32)", usage = "DISPLAY")
    protected String currencySymbol;
    @XmlElement(nillable = true)
    @CobolElement(cobolName = "envVarValues", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 3, minOccurs = 0, maxOccurs = 10, picture = "X(32)", usage = "DISPLAY")
    protected List<String> envVarValues;
    @CobolElement(cobolName = "formattedDate", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 3, picture = "X(32)", usage = "DISPLAY")
    protected String formattedDate;
    @CobolElement(cobolName = "language", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 3, picture = "X(32)", usage = "DISPLAY")
    protected String language;

    /**
     * Gets the value of the country property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCountry() {
        return country;
    }

    /**
     * Sets the value of the country property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCountry(String value) {
        this.country = value;
    }

    public boolean isSetCountry() {
        return (this.country!= null);
    }

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
     * Gets the value of the envVarValues property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the envVarValues property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getEnvVarValues().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link String }
     * 
     * 
     */
    public List<String> getEnvVarValues() {
        if (envVarValues == null) {
            envVarValues = new ArrayList<String>();
        }
        return this.envVarValues;
    }

    public boolean isSetEnvVarValues() {
        return ((this.envVarValues!= null)&&(!this.envVarValues.isEmpty()));
    }

    public void unsetEnvVarValues() {
        this.envVarValues = null;
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
     * Gets the value of the language property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLanguage() {
        return language;
    }

    /**
     * Sets the value of the language property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLanguage(String value) {
        this.language = value;
    }

    public boolean isSetLanguage() {
        return (this.language!= null);
    }

}


package com.legstar.test.coxb.osarrays;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for Dfhcommarea complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Dfhcommarea">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="SString">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="4"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="SBinary">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="4"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="AString" maxOccurs="2" minOccurs="2">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="4"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="ABinary" maxOccurs="2" minOccurs="2">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="4"/>
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
@XmlType(name = "Dfhcommarea", propOrder = {
    "sString",
    "sBinary",
    "aString",
    "aBinary"
})
public class Dfhcommarea
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "SString", required = true)
    @CobolElement(cobolName = "S-STRING", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(4)", srceLine = 24)
    protected String sString;
    @XmlElement(name = "SBinary", required = true)
    @CobolElement(cobolName = "S-BINARY", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(4)", srceLine = 25)
    protected String sBinary;
    @XmlElement(name = "AString", required = true)
    @CobolElement(cobolName = "A-STRING", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, minOccurs = 2, maxOccurs = 2, picture = "X(4)", srceLine = 27)
    protected List<String> aString;
    @XmlElement(name = "ABinary", required = true)
    @CobolElement(cobolName = "A-BINARY", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, minOccurs = 2, maxOccurs = 2, picture = "X(4)", srceLine = 28)
    protected List<String> aBinary;

    /**
     * Gets the value of the sString property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSString() {
        return sString;
    }

    /**
     * Sets the value of the sString property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSString(String value) {
        this.sString = value;
    }

    public boolean isSetSString() {
        return (this.sString!= null);
    }

    /**
     * Gets the value of the sBinary property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSBinary() {
        return sBinary;
    }

    /**
     * Sets the value of the sBinary property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSBinary(String value) {
        this.sBinary = value;
    }

    public boolean isSetSBinary() {
        return (this.sBinary!= null);
    }

    /**
     * Gets the value of the aString property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the aString property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getAString().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link String }
     * 
     * 
     */
    public List<String> getAString() {
        if (aString == null) {
            aString = new ArrayList<String>();
        }
        return this.aString;
    }

    public boolean isSetAString() {
        return ((this.aString!= null)&&(!this.aString.isEmpty()));
    }

    public void unsetAString() {
        this.aString = null;
    }

    /**
     * Gets the value of the aBinary property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the aBinary property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getABinary().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link String }
     * 
     * 
     */
    public List<String> getABinary() {
        if (aBinary == null) {
            aBinary = new ArrayList<String>();
        }
        return this.aBinary;
    }

    public boolean isSetABinary() {
        return ((this.aBinary!= null)&&(!this.aBinary.isEmpty()));
    }

    public void unsetABinary() {
        this.aBinary = null;
    }

}

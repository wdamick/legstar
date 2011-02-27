
package com.legstar.test.coxb.redopera;

import java.io.Serializable;
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
 *         &lt;element name="CFunction">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="18"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;choice>
 *           &lt;element name="CData">
 *             &lt;simpleType>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *                 &lt;maxLength value="200"/>
 *               &lt;/restriction>
 *             &lt;/simpleType>
 *           &lt;/element>
 *           &lt;element name="Filler25" type="{http://legstar.com/test/coxb/redopera}Filler25"/>
 *           &lt;element name="Filler28" type="{http://legstar.com/test/coxb/redopera}Filler28"/>
 *         &lt;/choice>
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
    "cFunction",
    "cData",
    "filler25",
    "filler28"
})
public class Dfhcommarea
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "CFunction", required = true)
    @CobolElement(cobolName = "C-FUNCTION", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(18)", srceLine = 21)
    protected String cFunction;
    @XmlElement(name = "CData")
    @CobolElement(cobolName = "C-DATA", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 3, isRedefined = true, picture = "X(200)", unmarshalChoiceStrategyClassName = "com.legstar.coxb.cust.redopera.ChoiceSelector", srceLine = 24)
    protected String cData;
    @XmlElement(name = "Filler25")
    @CobolElement(cobolName = "FILLER", type = CobolType.GROUP_ITEM, levelNumber = 3, redefines = "C-DATA", srceLine = 25)
    protected Filler25 filler25;
    @XmlElement(name = "Filler28")
    @CobolElement(cobolName = "FILLER", type = CobolType.GROUP_ITEM, levelNumber = 3, redefines = "C-DATA", srceLine = 28)
    protected Filler28 filler28;

    /**
     * Gets the value of the cFunction property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCFunction() {
        return cFunction;
    }

    /**
     * Sets the value of the cFunction property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCFunction(String value) {
        this.cFunction = value;
    }

    public boolean isSetCFunction() {
        return (this.cFunction!= null);
    }

    /**
     * Gets the value of the cData property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCData() {
        return cData;
    }

    /**
     * Sets the value of the cData property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCData(String value) {
        this.cData = value;
    }

    public boolean isSetCData() {
        return (this.cData!= null);
    }

    /**
     * Gets the value of the filler25 property.
     * 
     * @return
     *     possible object is
     *     {@link Filler25 }
     *     
     */
    public Filler25 getFiller25() {
        return filler25;
    }

    /**
     * Sets the value of the filler25 property.
     * 
     * @param value
     *     allowed object is
     *     {@link Filler25 }
     *     
     */
    public void setFiller25(Filler25 value) {
        this.filler25 = value;
    }

    public boolean isSetFiller25() {
        return (this.filler25 != null);
    }

    /**
     * Gets the value of the filler28 property.
     * 
     * @return
     *     possible object is
     *     {@link Filler28 }
     *     
     */
    public Filler28 getFiller28() {
        return filler28;
    }

    /**
     * Sets the value of the filler28 property.
     * 
     * @param value
     *     allowed object is
     *     {@link Filler28 }
     *     
     */
    public void setFiller28(Filler28 value) {
        this.filler28 = value;
    }

    public boolean isSetFiller28() {
        return (this.filler28 != null);
    }

}

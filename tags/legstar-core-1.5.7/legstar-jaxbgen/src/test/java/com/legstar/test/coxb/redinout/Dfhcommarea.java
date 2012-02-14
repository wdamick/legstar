
package com.legstar.test.coxb.redinout;

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
 *         &lt;element name="CNumeric">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *               &lt;totalDigits value="4"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;choice>
 *           &lt;element name="CBuffer">
 *             &lt;simpleType>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *                 &lt;maxLength value="500"/>
 *               &lt;/restriction>
 *             &lt;/simpleType>
 *           &lt;/element>
 *           &lt;element name="CParain" type="{http://legstar.com/test/coxb/redinout}CParain"/>
 *           &lt;element name="CParaout" type="{http://legstar.com/test/coxb/redinout}CParaout"/>
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
    "cNumeric",
    "cBuffer",
    "cParain",
    "cParaout"
})
public class Dfhcommarea
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "CNumeric")
    @CobolElement(cobolName = "C-NUMERIC", type = CobolType.BINARY_ITEM, levelNumber = 5, isSigned = false, totalDigits = 4, picture = "9(4)", usage = "BINARY", srceLine = 21)
    protected int cNumeric;
    @XmlElement(name = "CBuffer")
    @CobolElement(cobolName = "C-BUFFER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, isRedefined = true, picture = "X(500)", unmarshalChoiceStrategyClassName = "com.legstar.coxb.cust.redinout.ChoiceSelector", srceLine = 22)
    protected String cBuffer;
    @XmlElement(name = "CParain")
    @CobolElement(cobolName = "C-PARAIN", type = CobolType.GROUP_ITEM, levelNumber = 5, redefines = "C-BUFFER", srceLine = 23)
    protected CParain cParain;
    @XmlElement(name = "CParaout")
    @CobolElement(cobolName = "C-PARAOUT", type = CobolType.GROUP_ITEM, levelNumber = 5, redefines = "C-BUFFER", srceLine = 25)
    protected CParaout cParaout;

    /**
     * Gets the value of the cNumeric property.
     * 
     */
    public int getCNumeric() {
        return cNumeric;
    }

    /**
     * Sets the value of the cNumeric property.
     * 
     */
    public void setCNumeric(int value) {
        this.cNumeric = value;
    }

    public boolean isSetCNumeric() {
        return true;
    }

    /**
     * Gets the value of the cBuffer property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCBuffer() {
        return cBuffer;
    }

    /**
     * Sets the value of the cBuffer property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCBuffer(String value) {
        this.cBuffer = value;
    }

    public boolean isSetCBuffer() {
        return (this.cBuffer!= null);
    }

    /**
     * Gets the value of the cParain property.
     * 
     * @return
     *     possible object is
     *     {@link CParain }
     *     
     */
    public CParain getCParain() {
        return cParain;
    }

    /**
     * Sets the value of the cParain property.
     * 
     * @param value
     *     allowed object is
     *     {@link CParain }
     *     
     */
    public void setCParain(CParain value) {
        this.cParain = value;
    }

    public boolean isSetCParain() {
        return (this.cParain!= null);
    }

    /**
     * Gets the value of the cParaout property.
     * 
     * @return
     *     possible object is
     *     {@link CParaout }
     *     
     */
    public CParaout getCParaout() {
        return cParaout;
    }

    /**
     * Sets the value of the cParaout property.
     * 
     * @param value
     *     allowed object is
     *     {@link CParaout }
     *     
     */
    public void setCParaout(CParaout value) {
        this.cParaout = value;
    }

    public boolean isSetCParaout() {
        return (this.cParaout!= null);
    }

}


package com.legstar.test.coxb.valuemix;

import java.math.BigDecimal;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


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
 *         &lt;element name="WsZero" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="WsZeros" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="WsZeroes" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="WsSpace" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="WsSpaces" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="WsHighValue" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="WsHighValues" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="WsLowValue" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="WsLowValues" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="WsQuote" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="WsQuotes" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="WsNull" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="WsNulls" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="WsString" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="WsNumeric" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="WsPackedDecimal" type="{http://www.w3.org/2001/XMLSchema}decimal"/>
 *         &lt;element name="WsSingleFloat" type="{http://www.w3.org/2001/XMLSchema}float"/>
 *         &lt;element name="WsDoubleFloat" type="{http://www.w3.org/2001/XMLSchema}double"/>
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
    "wsZero",
    "wsZeros",
    "wsZeroes",
    "wsSpace",
    "wsSpaces",
    "wsHighValue",
    "wsHighValues",
    "wsLowValue",
    "wsLowValues",
    "wsQuote",
    "wsQuotes",
    "wsNull",
    "wsNulls",
    "wsString",
    "wsNumeric",
    "wsPackedDecimal",
    "wsSingleFloat",
    "wsDoubleFloat"
})
public class Dfhcommarea {

    @XmlElement(name = "WsZero")
    protected long wsZero;
    @XmlElement(name = "WsZeros")
    protected long wsZeros;
    @XmlElement(name = "WsZeroes")
    protected long wsZeroes;
    @XmlElement(name = "WsSpace", required = true)
    protected String wsSpace;
    @XmlElement(name = "WsSpaces", required = true)
    protected String wsSpaces;
    @XmlElement(name = "WsHighValue", required = true)
    protected String wsHighValue;
    @XmlElement(name = "WsHighValues", required = true)
    protected String wsHighValues;
    @XmlElement(name = "WsLowValue", required = true)
    protected String wsLowValue;
    @XmlElement(name = "WsLowValues", required = true)
    protected String wsLowValues;
    @XmlElement(name = "WsQuote", required = true)
    protected String wsQuote;
    @XmlElement(name = "WsQuotes", required = true)
    protected String wsQuotes;
    @XmlElement(name = "WsNull", required = true)
    protected String wsNull;
    @XmlElement(name = "WsNulls", required = true)
    protected String wsNulls;
    @XmlElement(name = "WsString", required = true)
    protected String wsString;
    @XmlElement(name = "WsNumeric")
    protected int wsNumeric;
    @XmlElement(name = "WsPackedDecimal", required = true)
    protected BigDecimal wsPackedDecimal;
    @XmlElement(name = "WsSingleFloat")
    protected float wsSingleFloat;
    @XmlElement(name = "WsDoubleFloat")
    protected double wsDoubleFloat;

    /**
     * Gets the value of the wsZero property.
     * 
     */
    public long getWsZero() {
        return wsZero;
    }

    /**
     * Sets the value of the wsZero property.
     * 
     */
    public void setWsZero(long value) {
        this.wsZero = value;
    }

    /**
     * Gets the value of the wsZeros property.
     * 
     */
    public long getWsZeros() {
        return wsZeros;
    }

    /**
     * Sets the value of the wsZeros property.
     * 
     */
    public void setWsZeros(long value) {
        this.wsZeros = value;
    }

    /**
     * Gets the value of the wsZeroes property.
     * 
     */
    public long getWsZeroes() {
        return wsZeroes;
    }

    /**
     * Sets the value of the wsZeroes property.
     * 
     */
    public void setWsZeroes(long value) {
        this.wsZeroes = value;
    }

    /**
     * Gets the value of the wsSpace property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getWsSpace() {
        return wsSpace;
    }

    /**
     * Sets the value of the wsSpace property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setWsSpace(String value) {
        this.wsSpace = value;
    }

    /**
     * Gets the value of the wsSpaces property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getWsSpaces() {
        return wsSpaces;
    }

    /**
     * Sets the value of the wsSpaces property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setWsSpaces(String value) {
        this.wsSpaces = value;
    }

    /**
     * Gets the value of the wsHighValue property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getWsHighValue() {
        return wsHighValue;
    }

    /**
     * Sets the value of the wsHighValue property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setWsHighValue(String value) {
        this.wsHighValue = value;
    }

    /**
     * Gets the value of the wsHighValues property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getWsHighValues() {
        return wsHighValues;
    }

    /**
     * Sets the value of the wsHighValues property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setWsHighValues(String value) {
        this.wsHighValues = value;
    }

    /**
     * Gets the value of the wsLowValue property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getWsLowValue() {
        return wsLowValue;
    }

    /**
     * Sets the value of the wsLowValue property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setWsLowValue(String value) {
        this.wsLowValue = value;
    }

    /**
     * Gets the value of the wsLowValues property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getWsLowValues() {
        return wsLowValues;
    }

    /**
     * Sets the value of the wsLowValues property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setWsLowValues(String value) {
        this.wsLowValues = value;
    }

    /**
     * Gets the value of the wsQuote property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getWsQuote() {
        return wsQuote;
    }

    /**
     * Sets the value of the wsQuote property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setWsQuote(String value) {
        this.wsQuote = value;
    }

    /**
     * Gets the value of the wsQuotes property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getWsQuotes() {
        return wsQuotes;
    }

    /**
     * Sets the value of the wsQuotes property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setWsQuotes(String value) {
        this.wsQuotes = value;
    }

    /**
     * Gets the value of the wsNull property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getWsNull() {
        return wsNull;
    }

    /**
     * Sets the value of the wsNull property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setWsNull(String value) {
        this.wsNull = value;
    }

    /**
     * Gets the value of the wsNulls property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getWsNulls() {
        return wsNulls;
    }

    /**
     * Sets the value of the wsNulls property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setWsNulls(String value) {
        this.wsNulls = value;
    }

    /**
     * Gets the value of the wsString property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getWsString() {
        return wsString;
    }

    /**
     * Sets the value of the wsString property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setWsString(String value) {
        this.wsString = value;
    }

    /**
     * Gets the value of the wsNumeric property.
     * 
     */
    public int getWsNumeric() {
        return wsNumeric;
    }

    /**
     * Sets the value of the wsNumeric property.
     * 
     */
    public void setWsNumeric(int value) {
        this.wsNumeric = value;
    }

    /**
     * Gets the value of the wsPackedDecimal property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getWsPackedDecimal() {
        return wsPackedDecimal;
    }

    /**
     * Sets the value of the wsPackedDecimal property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setWsPackedDecimal(BigDecimal value) {
        this.wsPackedDecimal = value;
    }

    /**
     * Gets the value of the wsSingleFloat property.
     * 
     */
    public float getWsSingleFloat() {
        return wsSingleFloat;
    }

    /**
     * Sets the value of the wsSingleFloat property.
     * 
     */
    public void setWsSingleFloat(float value) {
        this.wsSingleFloat = value;
    }

    /**
     * Gets the value of the wsDoubleFloat property.
     * 
     */
    public double getWsDoubleFloat() {
        return wsDoubleFloat;
    }

    /**
     * Sets the value of the wsDoubleFloat property.
     * 
     */
    public void setWsDoubleFloat(double value) {
        this.wsDoubleFloat = value;
    }

}

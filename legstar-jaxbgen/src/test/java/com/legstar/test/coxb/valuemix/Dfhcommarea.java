
package com.legstar.test.coxb.valuemix;

import java.io.Serializable;
import java.math.BigDecimal;
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
 *         &lt;element name="WsZero">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *               &lt;totalDigits value="5"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsZeros">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *               &lt;totalDigits value="5"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsZeroes">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *               &lt;totalDigits value="5"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsSpace">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="5"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsSpaces">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="5"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsHighValue">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="5"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsHighValues">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="5"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsLowValue">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="5"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsLowValues">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="5"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsQuote">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="5"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsQuotes">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="5"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsNull">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="5"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsNulls">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="5"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsString">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="5"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsNumeric">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}int">
 *               &lt;totalDigits value="5"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsPackedDecimal">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal">
 *               &lt;totalDigits value="17"/>
 *               &lt;fractionDigits value="2"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsSingleFloat">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}float">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsDoubleFloat">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}double">
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
public class Dfhcommarea
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "WsZero")
    @CobolElement(cobolName = "WS-ZERO", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 5, picture = "9(05)", value = "0", srceLine = 8)
    protected long wsZero = 0L;
    @XmlElement(name = "WsZeros")
    @CobolElement(cobolName = "WS-ZEROS", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 5, picture = "9(05)", value = "0", srceLine = 9)
    protected long wsZeros = 0L;
    @XmlElement(name = "WsZeroes")
    @CobolElement(cobolName = "WS-ZEROES", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 5, picture = "9(05)", value = "0", srceLine = 10)
    protected long wsZeroes = 0L;
    @XmlElement(name = "WsSpace", required = true)
    @CobolElement(cobolName = "WS-SPACE", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(05)", srceLine = 11)
    protected String wsSpace = "";
    @XmlElement(name = "WsSpaces", required = true)
    @CobolElement(cobolName = "WS-SPACES", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(05)", srceLine = 12)
    protected String wsSpaces = "";
    @XmlElement(name = "WsHighValue", required = true)
    @CobolElement(cobolName = "WS-HIGH-VALUE", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(05)", value = "0xFFFFFFFFFF", srceLine = 13)
    protected String wsHighValue = "0xFFFFFFFFFF";
    @XmlElement(name = "WsHighValues", required = true)
    @CobolElement(cobolName = "WS-HIGH-VALUES", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(05)", value = "0xFFFFFFFFFF", srceLine = 14)
    protected String wsHighValues = "0xFFFFFFFFFF";
    @XmlElement(name = "WsLowValue", required = true)
    @CobolElement(cobolName = "WS-LOW-VALUE", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(05)", value = "0x0000000000", srceLine = 15)
    protected String wsLowValue = "0x0000000000";
    @XmlElement(name = "WsLowValues", required = true)
    @CobolElement(cobolName = "WS-LOW-VALUES", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(05)", value = "0x0000000000", srceLine = 16)
    protected String wsLowValues = "0x0000000000";
    @XmlElement(name = "WsQuote", required = true)
    @CobolElement(cobolName = "WS-QUOTE", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(05)", value = "\'", srceLine = 17)
    protected String wsQuote = "\'";
    @XmlElement(name = "WsQuotes", required = true)
    @CobolElement(cobolName = "WS-QUOTES", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(05)", value = "\'", srceLine = 18)
    protected String wsQuotes = "\'";
    @XmlElement(name = "WsNull", required = true)
    @CobolElement(cobolName = "WS-NULL", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(05)", value = "0x0000000000", srceLine = 19)
    protected String wsNull = "0x0000000000";
    @XmlElement(name = "WsNulls", required = true)
    @CobolElement(cobolName = "WS-NULLS", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(05)", value = "0x0000000000", srceLine = 20)
    protected String wsNulls = "0x0000000000";
    @XmlElement(name = "WsString", required = true)
    @CobolElement(cobolName = "WS-STRING", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(05)", value = "ABCDE", srceLine = 21)
    protected String wsString = "ABCDE";
    @XmlElement(name = "WsNumeric")
    @CobolElement(cobolName = "WS-NUMERIC", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 5, isSigned = true, totalDigits = 5, picture = "S9(05)", value = "-345", srceLine = 22)
    protected int wsNumeric = -345;
    @XmlElement(name = "WsPackedDecimal", required = true)
    @CobolElement(cobolName = "WS-PACKED-DECIMAL", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 5, isSigned = true, totalDigits = 17, fractionDigits = 2, picture = "S9(15)V99", usage = "PACKED-DECIMAL", value = "-245.56", srceLine = 23)
    protected BigDecimal wsPackedDecimal = (new BigDecimal("-245.56"));
    @XmlElement(name = "WsSingleFloat")
    @CobolElement(cobolName = "WS-SINGLE-FLOAT", type = CobolType.SINGLE_FLOAT_ITEM, levelNumber = 5, usage = "COMP-1", value = "+0.6E08", srceLine = 24)
    protected float wsSingleFloat = 6.0E7F;
    @XmlElement(name = "WsDoubleFloat")
    @CobolElement(cobolName = "WS-DOUBLE-FLOAT", type = CobolType.DOUBLE_FLOAT_ITEM, levelNumber = 5, usage = "COMP-2", value = "-1.8E-56", srceLine = 25)
    protected double wsDoubleFloat = -1.8E-56D;

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

    public boolean isSetWsZero() {
        return true;
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

    public boolean isSetWsZeros() {
        return true;
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

    public boolean isSetWsZeroes() {
        return true;
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

    public boolean isSetWsSpace() {
        return (this.wsSpace!= null);
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

    public boolean isSetWsSpaces() {
        return (this.wsSpaces!= null);
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

    public boolean isSetWsHighValue() {
        return (this.wsHighValue!= null);
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

    public boolean isSetWsHighValues() {
        return (this.wsHighValues!= null);
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

    public boolean isSetWsLowValue() {
        return (this.wsLowValue!= null);
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

    public boolean isSetWsLowValues() {
        return (this.wsLowValues!= null);
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

    public boolean isSetWsQuote() {
        return (this.wsQuote!= null);
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

    public boolean isSetWsQuotes() {
        return (this.wsQuotes!= null);
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

    public boolean isSetWsNull() {
        return (this.wsNull!= null);
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

    public boolean isSetWsNulls() {
        return (this.wsNulls!= null);
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

    public boolean isSetWsString() {
        return (this.wsString!= null);
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

    public boolean isSetWsNumeric() {
        return true;
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

    public boolean isSetWsPackedDecimal() {
        return (this.wsPackedDecimal!= null);
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

    public boolean isSetWsSingleFloat() {
        return true;
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

    public boolean isSetWsDoubleFloat() {
        return true;
    }

}

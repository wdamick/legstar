
package com.legstar.test.coxb.rq071;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for RQ071Output complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="RQ071Output">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="rq071__error__code">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="3"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__total__bal">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal">
 *               &lt;minInclusive value="-99999999999.99"/>
 *               &lt;maxInclusive value="99999999999.99"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__cst__tp__cd">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="2"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__open__date">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="10"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__future__due">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal">
 *               &lt;minInclusive value="-99999999999.99"/>
 *               &lt;maxInclusive value="99999999999.99"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__current__due">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal">
 *               &lt;minInclusive value="-99999999999.99"/>
 *               &lt;maxInclusive value="99999999999.99"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__past__due__1">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal">
 *               &lt;minInclusive value="-99999999999.99"/>
 *               &lt;maxInclusive value="99999999999.99"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__past__due__2">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal">
 *               &lt;minInclusive value="-99999999999.99"/>
 *               &lt;maxInclusive value="99999999999.99"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__past__due__3">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal">
 *               &lt;minInclusive value="-99999999999.99"/>
 *               &lt;maxInclusive value="99999999999.99"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__cr__amnt">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal">
 *               &lt;minInclusive value="-99999999999.99"/>
 *               &lt;maxInclusive value="99999999999.99"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__total__bal__stmt">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal">
 *               &lt;minInclusive value="-99999999999.99"/>
 *               &lt;maxInclusive value="99999999999.99"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__future__due__stmt">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal">
 *               &lt;minInclusive value="-99999999999.99"/>
 *               &lt;maxInclusive value="99999999999.99"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__current__due__stmt">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal">
 *               &lt;minInclusive value="-99999999999.99"/>
 *               &lt;maxInclusive value="99999999999.99"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__past__due__1__stmt">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal">
 *               &lt;minInclusive value="-99999999999.99"/>
 *               &lt;maxInclusive value="99999999999.99"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__past__due__2__stmt">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal">
 *               &lt;minInclusive value="-99999999999.99"/>
 *               &lt;maxInclusive value="99999999999.99"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__past__due__3__stmt">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal">
 *               &lt;minInclusive value="-99999999999.99"/>
 *               &lt;maxInclusive value="99999999999.99"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__cr__amnt__stmt">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal">
 *               &lt;minInclusive value="-99999999999.99"/>
 *               &lt;maxInclusive value="99999999999.99"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__check" type="{http://creditstatus.customer.ibg/}Eb017output_rq071__check" maxOccurs="15" minOccurs="15"/>
 *         &lt;element name="rq071__last__stmnt__date">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="10"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__ar__term__desc">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="40"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__amnt__due">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal">
 *               &lt;minInclusive value="-99999999999.99"/>
 *               &lt;maxInclusive value="99999999999.99"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__curr__disc">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal">
 *               &lt;minInclusive value="-99999999999.99"/>
 *               &lt;maxInclusive value="99999999999.99"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__avail__disc">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal">
 *               &lt;minInclusive value="-99999999999.99"/>
 *               &lt;maxInclusive value="99999999999.99"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__due__date">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="10"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq071__final__due__amnt">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal">
 *               &lt;minInclusive value="-99999999999.99"/>
 *               &lt;maxInclusive value="99999999999.99"/>
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
@XmlType(name = "RQ071Output", propOrder = {
    "rq071__error__code",
    "rq071__total__bal",
    "rq071__cst__tp__cd",
    "rq071__open__date",
    "rq071__future__due",
    "rq071__current__due",
    "rq071__past__due__1",
    "rq071__past__due__2",
    "rq071__past__due__3",
    "rq071__cr__amnt",
    "rq071__total__bal__stmt",
    "rq071__future__due__stmt",
    "rq071__current__due__stmt",
    "rq071__past__due__1__stmt",
    "rq071__past__due__2__stmt",
    "rq071__past__due__3__stmt",
    "rq071__cr__amnt__stmt",
    "rq071__check",
    "rq071__last__stmnt__date",
    "rq071__ar__term__desc",
    "rq071__amnt__due",
    "rq071__curr__disc",
    "rq071__avail__disc",
    "rq071__due__date",
    "rq071__final__due__amnt"
})
public class RQ071Output
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--error--code", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 3, picture = "X(3)", usage = "DISPLAY")
    protected String rq071__error__code;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--total--bal", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 3, isSigned = true, totalDigits = 13, fractionDigits = 2, picture = "9(11)V9(2)", usage = "COMP-3")
    protected BigDecimal rq071__total__bal;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--cst--tp--cd", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 3, picture = "X(2)", usage = "DISPLAY")
    protected String rq071__cst__tp__cd;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--open--date", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 3, picture = "X(10)", usage = "DISPLAY")
    protected String rq071__open__date;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--future--due", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 3, isSigned = true, totalDigits = 13, fractionDigits = 2, picture = "9(11)V9(2)", usage = "COMP-3")
    protected BigDecimal rq071__future__due;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--current--due", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 3, isSigned = true, totalDigits = 13, fractionDigits = 2, picture = "9(11)V9(2)", usage = "COMP-3")
    protected BigDecimal rq071__current__due;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--past--due--1", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 3, isSigned = true, totalDigits = 13, fractionDigits = 2, picture = "9(11)V9(2)", usage = "COMP-3")
    protected BigDecimal rq071__past__due__1;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--past--due--2", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 3, isSigned = true, totalDigits = 13, fractionDigits = 2, picture = "9(11)V9(2)", usage = "COMP-3")
    protected BigDecimal rq071__past__due__2;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--past--due--3", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 3, isSigned = true, totalDigits = 13, fractionDigits = 2, picture = "9(11)V9(2)", usage = "COMP-3")
    protected BigDecimal rq071__past__due__3;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--cr--amnt", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 3, isSigned = true, totalDigits = 13, fractionDigits = 2, picture = "9(11)V9(2)", usage = "COMP-3")
    protected BigDecimal rq071__cr__amnt;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--total--bal--stmt", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 3, isSigned = true, totalDigits = 13, fractionDigits = 2, picture = "9(11)V9(2)", usage = "COMP-3")
    protected BigDecimal rq071__total__bal__stmt;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--future--due--stmt", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 3, isSigned = true, totalDigits = 13, fractionDigits = 2, picture = "9(11)V9(2)", usage = "COMP-3")
    protected BigDecimal rq071__future__due__stmt;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--current--due--stmt", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 3, isSigned = true, totalDigits = 13, fractionDigits = 2, picture = "9(11)V9(2)", usage = "COMP-3")
    protected BigDecimal rq071__current__due__stmt;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--past--due--1--stmt", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 3, isSigned = true, totalDigits = 13, fractionDigits = 2, picture = "9(11)V9(2)", usage = "COMP-3")
    protected BigDecimal rq071__past__due__1__stmt;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--past--due--2--stmt", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 3, isSigned = true, totalDigits = 13, fractionDigits = 2, picture = "9(11)V9(2)", usage = "COMP-3")
    protected BigDecimal rq071__past__due__2__stmt;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--past--due--3--stmt", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 3, isSigned = true, totalDigits = 13, fractionDigits = 2, picture = "9(11)V9(2)", usage = "COMP-3")
    protected BigDecimal rq071__past__due__3__stmt;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--cr--amnt--stmt", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 3, isSigned = true, totalDigits = 13, fractionDigits = 2, picture = "9(11)V9(2)", usage = "COMP-3")
    protected BigDecimal rq071__cr__amnt__stmt;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--check", type = CobolType.GROUP_ITEM, levelNumber = 3, minOccurs = 15, maxOccurs = 15)
    protected List<Eb017output_rq071__check> rq071__check;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--last--stmnt--date", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 3, picture = "X(10)", usage = "DISPLAY")
    protected String rq071__last__stmnt__date;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--ar--term--desc", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 3, picture = "X(40)", usage = "DISPLAY")
    protected String rq071__ar__term__desc;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--amnt--due", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 3, isSigned = true, totalDigits = 13, fractionDigits = 2, picture = "9(11)V9(2)", usage = "COMP-3")
    protected BigDecimal rq071__amnt__due;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--curr--disc", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 3, isSigned = true, totalDigits = 13, fractionDigits = 2, picture = "9(11)V9(2)", usage = "COMP-3")
    protected BigDecimal rq071__curr__disc;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--avail--disc", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 3, isSigned = true, totalDigits = 13, fractionDigits = 2, picture = "9(11)V9(2)", usage = "COMP-3")
    protected BigDecimal rq071__avail__disc;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--due--date", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 3, picture = "X(10)", usage = "DISPLAY")
    protected String rq071__due__date;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--final--due--amnt", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 3, isSigned = true, totalDigits = 13, fractionDigits = 2, picture = "9(11)V9(2)", usage = "COMP-3")
    protected BigDecimal rq071__final__due__amnt;

    /**
     * Gets the value of the rq071__error__code property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getRq071__error__code() {
        return rq071__error__code;
    }

    /**
     * Sets the value of the rq071__error__code property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setRq071__error__code(String value) {
        this.rq071__error__code = value;
    }

    public boolean isSetRq071__error__code() {
        return (this.rq071__error__code!= null);
    }

    /**
     * Gets the value of the rq071__total__bal property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getRq071__total__bal() {
        return rq071__total__bal;
    }

    /**
     * Sets the value of the rq071__total__bal property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setRq071__total__bal(BigDecimal value) {
        this.rq071__total__bal = value;
    }

    public boolean isSetRq071__total__bal() {
        return (this.rq071__total__bal!= null);
    }

    /**
     * Gets the value of the rq071__cst__tp__cd property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getRq071__cst__tp__cd() {
        return rq071__cst__tp__cd;
    }

    /**
     * Sets the value of the rq071__cst__tp__cd property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setRq071__cst__tp__cd(String value) {
        this.rq071__cst__tp__cd = value;
    }

    public boolean isSetRq071__cst__tp__cd() {
        return (this.rq071__cst__tp__cd!= null);
    }

    /**
     * Gets the value of the rq071__open__date property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getRq071__open__date() {
        return rq071__open__date;
    }

    /**
     * Sets the value of the rq071__open__date property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setRq071__open__date(String value) {
        this.rq071__open__date = value;
    }

    public boolean isSetRq071__open__date() {
        return (this.rq071__open__date!= null);
    }

    /**
     * Gets the value of the rq071__future__due property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getRq071__future__due() {
        return rq071__future__due;
    }

    /**
     * Sets the value of the rq071__future__due property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setRq071__future__due(BigDecimal value) {
        this.rq071__future__due = value;
    }

    public boolean isSetRq071__future__due() {
        return (this.rq071__future__due!= null);
    }

    /**
     * Gets the value of the rq071__current__due property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getRq071__current__due() {
        return rq071__current__due;
    }

    /**
     * Sets the value of the rq071__current__due property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setRq071__current__due(BigDecimal value) {
        this.rq071__current__due = value;
    }

    public boolean isSetRq071__current__due() {
        return (this.rq071__current__due!= null);
    }

    /**
     * Gets the value of the rq071__past__due__1 property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getRq071__past__due__1() {
        return rq071__past__due__1;
    }

    /**
     * Sets the value of the rq071__past__due__1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setRq071__past__due__1(BigDecimal value) {
        this.rq071__past__due__1 = value;
    }

    public boolean isSetRq071__past__due__1() {
        return (this.rq071__past__due__1 != null);
    }

    /**
     * Gets the value of the rq071__past__due__2 property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getRq071__past__due__2() {
        return rq071__past__due__2;
    }

    /**
     * Sets the value of the rq071__past__due__2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setRq071__past__due__2(BigDecimal value) {
        this.rq071__past__due__2 = value;
    }

    public boolean isSetRq071__past__due__2() {
        return (this.rq071__past__due__2 != null);
    }

    /**
     * Gets the value of the rq071__past__due__3 property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getRq071__past__due__3() {
        return rq071__past__due__3;
    }

    /**
     * Sets the value of the rq071__past__due__3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setRq071__past__due__3(BigDecimal value) {
        this.rq071__past__due__3 = value;
    }

    public boolean isSetRq071__past__due__3() {
        return (this.rq071__past__due__3 != null);
    }

    /**
     * Gets the value of the rq071__cr__amnt property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getRq071__cr__amnt() {
        return rq071__cr__amnt;
    }

    /**
     * Sets the value of the rq071__cr__amnt property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setRq071__cr__amnt(BigDecimal value) {
        this.rq071__cr__amnt = value;
    }

    public boolean isSetRq071__cr__amnt() {
        return (this.rq071__cr__amnt!= null);
    }

    /**
     * Gets the value of the rq071__total__bal__stmt property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getRq071__total__bal__stmt() {
        return rq071__total__bal__stmt;
    }

    /**
     * Sets the value of the rq071__total__bal__stmt property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setRq071__total__bal__stmt(BigDecimal value) {
        this.rq071__total__bal__stmt = value;
    }

    public boolean isSetRq071__total__bal__stmt() {
        return (this.rq071__total__bal__stmt!= null);
    }

    /**
     * Gets the value of the rq071__future__due__stmt property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getRq071__future__due__stmt() {
        return rq071__future__due__stmt;
    }

    /**
     * Sets the value of the rq071__future__due__stmt property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setRq071__future__due__stmt(BigDecimal value) {
        this.rq071__future__due__stmt = value;
    }

    public boolean isSetRq071__future__due__stmt() {
        return (this.rq071__future__due__stmt!= null);
    }

    /**
     * Gets the value of the rq071__current__due__stmt property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getRq071__current__due__stmt() {
        return rq071__current__due__stmt;
    }

    /**
     * Sets the value of the rq071__current__due__stmt property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setRq071__current__due__stmt(BigDecimal value) {
        this.rq071__current__due__stmt = value;
    }

    public boolean isSetRq071__current__due__stmt() {
        return (this.rq071__current__due__stmt!= null);
    }

    /**
     * Gets the value of the rq071__past__due__1__stmt property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getRq071__past__due__1__stmt() {
        return rq071__past__due__1__stmt;
    }

    /**
     * Sets the value of the rq071__past__due__1__stmt property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setRq071__past__due__1__stmt(BigDecimal value) {
        this.rq071__past__due__1__stmt = value;
    }

    public boolean isSetRq071__past__due__1__stmt() {
        return (this.rq071__past__due__1__stmt!= null);
    }

    /**
     * Gets the value of the rq071__past__due__2__stmt property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getRq071__past__due__2__stmt() {
        return rq071__past__due__2__stmt;
    }

    /**
     * Sets the value of the rq071__past__due__2__stmt property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setRq071__past__due__2__stmt(BigDecimal value) {
        this.rq071__past__due__2__stmt = value;
    }

    public boolean isSetRq071__past__due__2__stmt() {
        return (this.rq071__past__due__2__stmt!= null);
    }

    /**
     * Gets the value of the rq071__past__due__3__stmt property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getRq071__past__due__3__stmt() {
        return rq071__past__due__3__stmt;
    }

    /**
     * Sets the value of the rq071__past__due__3__stmt property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setRq071__past__due__3__stmt(BigDecimal value) {
        this.rq071__past__due__3__stmt = value;
    }

    public boolean isSetRq071__past__due__3__stmt() {
        return (this.rq071__past__due__3__stmt!= null);
    }

    /**
     * Gets the value of the rq071__cr__amnt__stmt property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getRq071__cr__amnt__stmt() {
        return rq071__cr__amnt__stmt;
    }

    /**
     * Sets the value of the rq071__cr__amnt__stmt property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setRq071__cr__amnt__stmt(BigDecimal value) {
        this.rq071__cr__amnt__stmt = value;
    }

    public boolean isSetRq071__cr__amnt__stmt() {
        return (this.rq071__cr__amnt__stmt!= null);
    }

    /**
     * 
     * 
     * @return
     *     array of
     *     {@link Eb017output_rq071__check }
     *     
     */
    public Eb017output_rq071__check[] getRq071__check() {
        if (this.rq071__check == null) {
            return new Eb017output_rq071__check[ 0 ] ;
        }
        return ((Eb017output_rq071__check[]) this.rq071__check.toArray(new Eb017output_rq071__check[this.rq071__check.size()] ));
    }

    /**
     * 
     * 
     * @return
     *     one of
     *     {@link Eb017output_rq071__check }
     *     
     */
    public Eb017output_rq071__check getRq071__check(int idx) {
        if (this.rq071__check == null) {
            throw new IndexOutOfBoundsException();
        }
        return this.rq071__check.get(idx);
    }

    public int getRq071__checkLength() {
        if (this.rq071__check == null) {
            return  0;
        }
        return this.rq071__check.size();
    }

    /**
     * 
     * 
     * @param values
     *     allowed objects are
     *     {@link Eb017output_rq071__check }
     *     
     */
    public void setRq071__check(Eb017output_rq071__check[] values) {
        this._getRq071__check().clear();
        int len = values.length;
        for (int i = 0; (i<len); i ++) {
            this.rq071__check.add(values[i]);
        }
    }

    protected List<Eb017output_rq071__check> _getRq071__check() {
        if (rq071__check == null) {
            rq071__check = new ArrayList<Eb017output_rq071__check>();
        }
        return rq071__check;
    }

    /**
     * 
     * 
     * @param value
     *     allowed object is
     *     {@link Eb017output_rq071__check }
     *     
     */
    public Eb017output_rq071__check setRq071__check(int idx, Eb017output_rq071__check value) {
        return this.rq071__check.set(idx, value);
    }

    public boolean isSetRq071__check() {
        return ((this.rq071__check!= null)&&(!this.rq071__check.isEmpty()));
    }

    public void unsetRq071__check() {
        this.rq071__check = null;
    }

    /**
     * Gets the value of the rq071__last__stmnt__date property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getRq071__last__stmnt__date() {
        return rq071__last__stmnt__date;
    }

    /**
     * Sets the value of the rq071__last__stmnt__date property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setRq071__last__stmnt__date(String value) {
        this.rq071__last__stmnt__date = value;
    }

    public boolean isSetRq071__last__stmnt__date() {
        return (this.rq071__last__stmnt__date!= null);
    }

    /**
     * Gets the value of the rq071__ar__term__desc property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getRq071__ar__term__desc() {
        return rq071__ar__term__desc;
    }

    /**
     * Sets the value of the rq071__ar__term__desc property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setRq071__ar__term__desc(String value) {
        this.rq071__ar__term__desc = value;
    }

    public boolean isSetRq071__ar__term__desc() {
        return (this.rq071__ar__term__desc!= null);
    }

    /**
     * Gets the value of the rq071__amnt__due property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getRq071__amnt__due() {
        return rq071__amnt__due;
    }

    /**
     * Sets the value of the rq071__amnt__due property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setRq071__amnt__due(BigDecimal value) {
        this.rq071__amnt__due = value;
    }

    public boolean isSetRq071__amnt__due() {
        return (this.rq071__amnt__due!= null);
    }

    /**
     * Gets the value of the rq071__curr__disc property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getRq071__curr__disc() {
        return rq071__curr__disc;
    }

    /**
     * Sets the value of the rq071__curr__disc property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setRq071__curr__disc(BigDecimal value) {
        this.rq071__curr__disc = value;
    }

    public boolean isSetRq071__curr__disc() {
        return (this.rq071__curr__disc!= null);
    }

    /**
     * Gets the value of the rq071__avail__disc property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getRq071__avail__disc() {
        return rq071__avail__disc;
    }

    /**
     * Sets the value of the rq071__avail__disc property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setRq071__avail__disc(BigDecimal value) {
        this.rq071__avail__disc = value;
    }

    public boolean isSetRq071__avail__disc() {
        return (this.rq071__avail__disc!= null);
    }

    /**
     * Gets the value of the rq071__due__date property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getRq071__due__date() {
        return rq071__due__date;
    }

    /**
     * Sets the value of the rq071__due__date property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setRq071__due__date(String value) {
        this.rq071__due__date = value;
    }

    public boolean isSetRq071__due__date() {
        return (this.rq071__due__date!= null);
    }

    /**
     * Gets the value of the rq071__final__due__amnt property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getRq071__final__due__amnt() {
        return rq071__final__due__amnt;
    }

    /**
     * Sets the value of the rq071__final__due__amnt property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setRq071__final__due__amnt(BigDecimal value) {
        this.rq071__final__due__amnt = value;
    }

    public boolean isSetRq071__final__due__amnt() {
        return (this.rq071__final__due__amnt!= null);
    }

}

package com.legstar.test.coxb.pojo156.jaxb;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;

import com.legstar.coxb.CobolComplexType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;

/**
 * <p>
 * Java class for someItem complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="someItem">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="amount" type="{http://www.w3.org/2001/XMLSchema}decimal" minOccurs="0"/>
 *         &lt;element name="label" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="number" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "someItem", propOrder = { "amount", "label", "number" })
@CobolComplexType(javaClassName = "com.legstar.java.SomeItem")
public class SomeItem implements Serializable {

    private final static long serialVersionUID = 1L;
    @CobolElement(cobolName = "amount", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 5, isSigned = true, totalDigits = 9, fractionDigits = 2, picture = "S9(7)V9(2)", usage = "COMP-3")
    protected BigDecimal amount;
    @CobolElement(cobolName = "R-label", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(32)", usage = "DISPLAY")
    protected String label;
    @CobolElement(cobolName = "R-number", type = CobolType.BINARY_ITEM, levelNumber = 5, isSigned = true, totalDigits = 9, picture = "S9(9)", usage = "COMP-5")
    protected int number;

    /**
     * Gets the value of the amount property.
     * 
     * @return possible object is {@link BigDecimal }
     * 
     */
    public BigDecimal getAmount() {
        return amount;
    }

    /**
     * Sets the value of the amount property.
     * 
     * @param value allowed object is {@link BigDecimal }
     * 
     */
    public void setAmount(BigDecimal value) {
        this.amount = value;
    }

    public boolean isSetAmount() {
        return (this.amount != null);
    }

    /**
     * Gets the value of the label property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getLabel() {
        return label;
    }

    /**
     * Sets the value of the label property.
     * 
     * @param value allowed object is {@link String }
     * 
     */
    public void setLabel(String value) {
        this.label = value;
    }

    public boolean isSetLabel() {
        return (this.label != null);
    }

    /**
     * Gets the value of the number property.
     * 
     */
    public int getNumber() {
        return number;
    }

    /**
     * Sets the value of the number property.
     * 
     */
    public void setNumber(int value) {
        this.number = value;
    }

    public boolean isSetNumber() {
        return true;
    }

}

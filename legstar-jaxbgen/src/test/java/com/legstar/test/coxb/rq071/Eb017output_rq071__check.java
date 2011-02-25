
package com.legstar.test.coxb.rq071;

import java.io.Serializable;
import java.math.BigDecimal;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for Eb017output_rq071__check complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Eb017output_rq071__check">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;group ref="{http://creditstatus.customer.ibg/}rq071output_rq071__check"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Eb017output_rq071__check", propOrder = {
    "rq071__check__date",
    "rq071__check__no",
    "rq071__check__amnt"
})
public class Eb017output_rq071__check
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--check--date", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(10)", usage = "DISPLAY")
    protected String rq071__check__date;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--check--no", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(12)", usage = "DISPLAY")
    protected String rq071__check__no;
    @XmlElement(required = true)
    @CobolElement(cobolName = "rq071--check--amnt", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 5, isSigned = true, totalDigits = 13, fractionDigits = 2, picture = "9(11)V9(2)", usage = "COMP-3")
    protected BigDecimal rq071__check__amnt;

    /**
     * Gets the value of the rq071__check__date property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getRq071__check__date() {
        return rq071__check__date;
    }

    /**
     * Sets the value of the rq071__check__date property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setRq071__check__date(String value) {
        this.rq071__check__date = value;
    }

    public boolean isSetRq071__check__date() {
        return (this.rq071__check__date!= null);
    }

    /**
     * Gets the value of the rq071__check__no property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getRq071__check__no() {
        return rq071__check__no;
    }

    /**
     * Sets the value of the rq071__check__no property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setRq071__check__no(String value) {
        this.rq071__check__no = value;
    }

    public boolean isSetRq071__check__no() {
        return (this.rq071__check__no!= null);
    }

    /**
     * Gets the value of the rq071__check__amnt property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getRq071__check__amnt() {
        return rq071__check__amnt;
    }

    /**
     * Sets the value of the rq071__check__amnt property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setRq071__check__amnt(BigDecimal value) {
        this.rq071__check__amnt = value;
    }

    public boolean isSetRq071__check__amnt() {
        return (this.rq071__check__amnt!= null);
    }

}

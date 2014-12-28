
package com.legstar.test.coxb.issue185;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for OuterRedefinesShort complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="OuterRedefinesShort">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;choice>
 *           &lt;element name="innerRedefinesLong">
 *             &lt;simpleType>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *                 &lt;maxLength value="5"/>
 *               &lt;/restriction>
 *             &lt;/simpleType>
 *           &lt;/element>
 *           &lt;element name="innerRedefinesShort">
 *             &lt;simpleType>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *                 &lt;maxLength value="3"/>
 *               &lt;/restriction>
 *             &lt;/simpleType>
 *           &lt;/element>
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
@XmlType(name = "OuterRedefinesShort", propOrder = {
    "innerRedefinesLong",
    "innerRedefinesShort"
})
public class OuterRedefinesShort
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @CobolElement(cobolName = "INNER-REDEFINES-LONG", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 15, isRedefined = true, picture = "X(5)", srceLine = 5)
    protected String innerRedefinesLong;
    @CobolElement(cobolName = "INNER-REDEFINES-SHORT", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 15, redefines = "INNER-REDEFINES-LONG", picture = "X(3)", srceLine = 6)
    protected String innerRedefinesShort;

    /**
     * Gets the value of the innerRedefinesLong property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getInnerRedefinesLong() {
        return innerRedefinesLong;
    }

    /**
     * Sets the value of the innerRedefinesLong property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setInnerRedefinesLong(String value) {
        this.innerRedefinesLong = value;
    }

    public boolean isSetInnerRedefinesLong() {
        return (this.innerRedefinesLong!= null);
    }

    /**
     * Gets the value of the innerRedefinesShort property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getInnerRedefinesShort() {
        return innerRedefinesShort;
    }

    /**
     * Sets the value of the innerRedefinesShort property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setInnerRedefinesShort(String value) {
        this.innerRedefinesShort = value;
    }

    public boolean isSetInnerRedefinesShort() {
        return (this.innerRedefinesShort!= null);
    }

}

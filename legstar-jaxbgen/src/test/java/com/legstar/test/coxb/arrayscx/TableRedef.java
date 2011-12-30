
package com.legstar.test.coxb.arrayscx;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for TableRedef complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="TableRedef">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;choice>
 *           &lt;element name="ElementRedef1">
 *             &lt;simpleType>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *                 &lt;maxLength value="3"/>
 *               &lt;/restriction>
 *             &lt;/simpleType>
 *           &lt;/element>
 *           &lt;element name="ElementRedef2">
 *             &lt;simpleType>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *                 &lt;totalDigits value="3"/>
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
@XmlType(name = "TableRedef", propOrder = {
    "elementRedef1",
    "elementRedef2"
})
public class TableRedef
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "ElementRedef1")
    @CobolElement(cobolName = "ELEMENT-REDEF-1", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 10, isRedefined = true, picture = "X(3)", unmarshalChoiceStrategyClassName = "com.legstar.coxb.cust.arrayscx.ChoiceSelector", srceLine = 34)
    protected String elementRedef1;
    @XmlElement(name = "ElementRedef2")
    @CobolElement(cobolName = "ELEMENT-REDEF-2", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 10, isSigned = false, totalDigits = 3, redefines = "ELEMENT-REDEF-1", picture = "9(3)", srceLine = 35)
    protected Integer elementRedef2;

    /**
     * Gets the value of the elementRedef1 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getElementRedef1() {
        return elementRedef1;
    }

    /**
     * Sets the value of the elementRedef1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setElementRedef1(String value) {
        this.elementRedef1 = value;
    }

    public boolean isSetElementRedef1() {
        return (this.elementRedef1 != null);
    }

    /**
     * Gets the value of the elementRedef2 property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getElementRedef2() {
        return elementRedef2;
    }

    /**
     * Sets the value of the elementRedef2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setElementRedef2(Integer value) {
        this.elementRedef2 = value;
    }

    public boolean isSetElementRedef2() {
        return (this.elementRedef2 != null);
    }

}

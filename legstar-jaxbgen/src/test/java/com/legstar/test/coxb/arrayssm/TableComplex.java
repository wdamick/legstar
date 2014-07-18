
package com.legstar.test.coxb.arrayssm;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for TableComplex complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="TableComplex">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ElementComplex">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="5"/>
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
@XmlType(name = "TableComplex", propOrder = {
    "elementComplex"
})
public class TableComplex
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "ElementComplex", required = true)
    @CobolElement(cobolName = "ELEMENT-COMPLEX", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 10, picture = "X(5)", srceLine = 34)
    protected String elementComplex;

    /**
     * Gets the value of the elementComplex property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getElementComplex() {
        return elementComplex;
    }

    /**
     * Sets the value of the elementComplex property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setElementComplex(String value) {
        this.elementComplex = value;
    }

    public boolean isSetElementComplex() {
        return (this.elementComplex!= null);
    }

}

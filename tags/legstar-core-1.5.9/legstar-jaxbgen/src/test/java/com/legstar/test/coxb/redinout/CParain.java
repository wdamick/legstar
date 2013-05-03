
package com.legstar.test.coxb.redinout;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for CParain complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="CParain">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CSomeInput">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="15"/>
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
@XmlType(name = "CParain", propOrder = {
    "cSomeInput"
})
public class CParain
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "CSomeInput", required = true)
    @CobolElement(cobolName = "C-SOME-INPUT", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 10, picture = "X(15)", srceLine = 24)
    protected String cSomeInput;

    /**
     * Gets the value of the cSomeInput property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCSomeInput() {
        return cSomeInput;
    }

    /**
     * Sets the value of the cSomeInput property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCSomeInput(String value) {
        this.cSomeInput = value;
    }

    public boolean isSetCSomeInput() {
        return (this.cSomeInput!= null);
    }

}

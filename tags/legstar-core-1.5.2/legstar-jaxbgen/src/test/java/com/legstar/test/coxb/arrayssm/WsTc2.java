
package com.legstar.test.coxb.arrayssm;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for WsTc2 complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WsTc2">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="Filler27">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="6"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsTc2A">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *               &lt;totalDigits value="1"/>
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
@XmlType(name = "WsTc2", propOrder = {
    "filler27",
    "wsTc2A"
})
public class WsTc2
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "Filler27", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(6)", value = "TC2EC2", srceLine = 27)
    protected String filler27 = "TC2EC2";
    @XmlElement(name = "WsTc2A")
    @CobolElement(cobolName = "WS-TC2-A", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 1, picture = "9(1)", srceLine = 28)
    protected int wsTc2A;

    /**
     * Gets the value of the filler27 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller27() {
        return filler27;
    }

    /**
     * Sets the value of the filler27 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller27(String value) {
        this.filler27 = value;
    }

    public boolean isSetFiller27() {
        return (this.filler27 != null);
    }

    /**
     * Gets the value of the wsTc2A property.
     * 
     */
    public int getWsTc2A() {
        return wsTc2A;
    }

    /**
     * Sets the value of the wsTc2A property.
     * 
     */
    public void setWsTc2A(int value) {
        this.wsTc2A = value;
    }

    public boolean isSetWsTc2A() {
        return true;
    }

}

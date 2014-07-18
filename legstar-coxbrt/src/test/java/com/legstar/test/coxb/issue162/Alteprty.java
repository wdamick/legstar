
package com.legstar.test.coxb.issue162;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for Alteprty complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Alteprty">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="altePrtyQual">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="4"/>
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
@XmlType(name = "Alteprty", propOrder = {
    "altePrtyQual"
})
public class Alteprty
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(required = true)
    @CobolElement(cobolName = "ALTE-PRTY-QUAL", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 15, picture = "X(04)", srceLine = 10)
    protected String altePrtyQual;

    /**
     * Gets the value of the altePrtyQual property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAltePrtyQual() {
        return altePrtyQual;
    }

    /**
     * Sets the value of the altePrtyQual property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAltePrtyQual(String value) {
        this.altePrtyQual = value;
    }

    public boolean isSetAltePrtyQual() {
        return (this.altePrtyQual!= null);
    }

}

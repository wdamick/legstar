
package com.legstar.test.coxb.lsfileae;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for ComPersonal complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ComPersonal">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ComName">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="20"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="ComAddress">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="20"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="ComPhone">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="8"/>
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
@XmlType(name = "ComPersonal", propOrder = {
    "comName",
    "comAddress",
    "comPhone"
})
public class ComPersonal
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "ComName", required = true)
    @CobolElement(cobolName = "COM-NAME", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 10, picture = "X(20)", srceLine = 34)
    protected String comName;
    @XmlElement(name = "ComAddress", required = true)
    @CobolElement(cobolName = "COM-ADDRESS", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 10, picture = "X(20)", srceLine = 35)
    protected String comAddress;
    @XmlElement(name = "ComPhone", required = true)
    @CobolElement(cobolName = "COM-PHONE", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 10, picture = "X(8)", srceLine = 36)
    protected String comPhone;

    /**
     * Gets the value of the comName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getComName() {
        return comName;
    }

    /**
     * Sets the value of the comName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setComName(String value) {
        this.comName = value;
    }

    public boolean isSetComName() {
        return (this.comName!= null);
    }

    /**
     * Gets the value of the comAddress property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getComAddress() {
        return comAddress;
    }

    /**
     * Sets the value of the comAddress property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setComAddress(String value) {
        this.comAddress = value;
    }

    public boolean isSetComAddress() {
        return (this.comAddress!= null);
    }

    /**
     * Gets the value of the comPhone property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getComPhone() {
        return comPhone;
    }

    /**
     * Sets the value of the comPhone property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setComPhone(String value) {
        this.comPhone = value;
    }

    public boolean isSetComPhone() {
        return (this.comPhone!= null);
    }

}


package com.legstar.test.coxb.varar021;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for LkupInfo44 complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LkupInfo44">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LkupIdCt">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="18"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="LkupTypCdCt">
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
@XmlType(name = "LkupInfo44", propOrder = {
    "lkupIdCt",
    "lkupTypCdCt"
})
public class LkupInfo44
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "LkupIdCt", required = true)
    @CobolElement(cobolName = "LKUP-ID-CT", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 15, picture = "X(18)", srceLine = 45)
    protected String lkupIdCt;
    @XmlElement(name = "LkupTypCdCt", required = true)
    @CobolElement(cobolName = "LKUP-TYP-CD-CT", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 15, picture = "X(05)", srceLine = 46)
    protected String lkupTypCdCt;

    /**
     * Gets the value of the lkupIdCt property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLkupIdCt() {
        return lkupIdCt;
    }

    /**
     * Sets the value of the lkupIdCt property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLkupIdCt(String value) {
        this.lkupIdCt = value;
    }

    public boolean isSetLkupIdCt() {
        return (this.lkupIdCt!= null);
    }

    /**
     * Gets the value of the lkupTypCdCt property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLkupTypCdCt() {
        return lkupTypCdCt;
    }

    /**
     * Sets the value of the lkupTypCdCt property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLkupTypCdCt(String value) {
        this.lkupTypCdCt = value;
    }

    public boolean isSetLkupTypCdCt() {
        return (this.lkupTypCdCt!= null);
    }

}

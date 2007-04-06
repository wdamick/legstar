
package com.legstar.test.coxb.charsets;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.HexBinaryAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;


/**
 * <p>Java class for DfhcommareaType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="DfhcommareaType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ComLocal" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="ComDbcs" type="{http://www.w3.org/2001/XMLSchema}hexBinary"/>
 *         &lt;element name="ComNational" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "DfhcommareaType", propOrder = {
    "comLocal",
    "comDbcs",
    "comNational"
})
public class DfhcommareaType {

    @XmlElement(name = "ComLocal", required = true)
    protected String comLocal;
    @XmlElement(name = "ComDbcs", required = true, type = String.class)
    @XmlJavaTypeAdapter(HexBinaryAdapter.class)
    @XmlSchemaType(name = "hexBinary")
    protected byte[] comDbcs;
    @XmlElement(name = "ComNational", required = true)
    protected String comNational;

    /**
     * Gets the value of the comLocal property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getComLocal() {
        return comLocal;
    }

    /**
     * Sets the value of the comLocal property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setComLocal(String value) {
        this.comLocal = value;
    }

    /**
     * Gets the value of the comDbcs property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public byte[] getComDbcs() {
        return comDbcs;
    }

    /**
     * Sets the value of the comDbcs property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setComDbcs(byte[] value) {
        this.comDbcs = ((byte[]) value);
    }

    /**
     * Gets the value of the comNational property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getComNational() {
        return comNational;
    }

    /**
     * Sets the value of the comNational property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setComNational(String value) {
        this.comNational = value;
    }

}

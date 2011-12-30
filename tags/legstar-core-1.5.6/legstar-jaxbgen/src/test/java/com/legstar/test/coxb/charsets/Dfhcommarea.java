
package com.legstar.test.coxb.charsets;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for Dfhcommarea complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Dfhcommarea">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ComLocal">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="32"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="ComDbcs">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="32"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="ComNational">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="32"/>
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
@XmlType(name = "Dfhcommarea", propOrder = {
    "comLocal",
    "comDbcs",
    "comNational"
})
public class Dfhcommarea
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "ComLocal", required = true)
    @CobolElement(cobolName = "COM-LOCAL", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(32)", srceLine = 43)
    protected String comLocal;
    @XmlElement(name = "ComDbcs", required = true)
    @CobolElement(cobolName = "COM-DBCS", type = CobolType.DBCS_ITEM, levelNumber = 5, picture = "G(32)", usage = "DISPLAY-1", srceLine = 44)
    protected String comDbcs;
    @XmlElement(name = "ComNational", required = true)
    @CobolElement(cobolName = "COM-NATIONAL", type = CobolType.NATIONAL_ITEM, levelNumber = 5, picture = "N(32)", usage = "NATIONAL", srceLine = 45)
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

    public boolean isSetComLocal() {
        return (this.comLocal!= null);
    }

    /**
     * Gets the value of the comDbcs property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getComDbcs() {
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
    public void setComDbcs(String value) {
        this.comDbcs = value;
    }

    public boolean isSetComDbcs() {
        return (this.comDbcs!= null);
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

    public boolean isSetComNational() {
        return (this.comNational!= null);
    }

}


package com.legstar.test.coxb.lsfileae;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


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
 *         &lt;element name="ComNumber" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="ComPersonal" type="{http://legstar.com/test/coxb/lsfileae}ComPersonal"/>
 *         &lt;element name="ComDate" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="ComAmount" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="ComComment" type="{http://www.w3.org/2001/XMLSchema}string"/>
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
    "comNumber",
    "comPersonal",
    "comDate",
    "comAmount",
    "comComment"
})
public class Dfhcommarea {

    @XmlElement(name = "ComNumber")
    protected long comNumber;
    @XmlElement(name = "ComPersonal", required = true)
    protected ComPersonal comPersonal;
    @XmlElement(name = "ComDate", required = true)
    protected String comDate;
    @XmlElement(name = "ComAmount", required = true)
    protected String comAmount;
    @XmlElement(name = "ComComment", required = true)
    protected String comComment;

    /**
     * Gets the value of the comNumber property.
     * 
     */
    public long getComNumber() {
        return comNumber;
    }

    /**
     * Sets the value of the comNumber property.
     * 
     */
    public void setComNumber(long value) {
        this.comNumber = value;
    }

    /**
     * Gets the value of the comPersonal property.
     * 
     * @return
     *     possible object is
     *     {@link ComPersonal }
     *     
     */
    public ComPersonal getComPersonal() {
        return comPersonal;
    }

    /**
     * Sets the value of the comPersonal property.
     * 
     * @param value
     *     allowed object is
     *     {@link ComPersonal }
     *     
     */
    public void setComPersonal(ComPersonal value) {
        this.comPersonal = value;
    }

    /**
     * Gets the value of the comDate property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getComDate() {
        return comDate;
    }

    /**
     * Sets the value of the comDate property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setComDate(String value) {
        this.comDate = value;
    }

    /**
     * Gets the value of the comAmount property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getComAmount() {
        return comAmount;
    }

    /**
     * Sets the value of the comAmount property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setComAmount(String value) {
        this.comAmount = value;
    }

    /**
     * Gets the value of the comComment property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getComComment() {
        return comComment;
    }

    /**
     * Sets the value of the comComment property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setComComment(String value) {
        this.comComment = value;
    }

}

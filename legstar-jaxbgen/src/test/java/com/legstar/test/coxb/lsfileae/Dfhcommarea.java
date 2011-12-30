
package com.legstar.test.coxb.lsfileae;

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
 *         &lt;element name="ComNumber">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *               &lt;totalDigits value="6"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="ComPersonal" type="{http://legstar.com/test/coxb/lsfileae}ComPersonal"/>
 *         &lt;element name="ComDate">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="ComAmount">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="ComComment">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="9"/>
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
    "comNumber",
    "comPersonal",
    "comDate",
    "comAmount",
    "comComment"
})
public class Dfhcommarea
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "ComNumber")
    @CobolElement(cobolName = "COM-NUMBER", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 6, picture = "9(6)", srceLine = 32)
    protected long comNumber;
    @XmlElement(name = "ComPersonal", required = true)
    @CobolElement(cobolName = "COM-PERSONAL", type = CobolType.GROUP_ITEM, levelNumber = 5, srceLine = 33)
    protected ComPersonal comPersonal;
    @XmlElement(name = "ComDate", required = true)
    @CobolElement(cobolName = "COM-DATE", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(8)", srceLine = 37)
    protected String comDate;
    @XmlElement(name = "ComAmount", required = true)
    @CobolElement(cobolName = "COM-AMOUNT", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(8)", srceLine = 38)
    protected String comAmount;
    @XmlElement(name = "ComComment", required = true)
    @CobolElement(cobolName = "COM-COMMENT", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(9)", srceLine = 39)
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

    public boolean isSetComNumber() {
        return true;
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

    public boolean isSetComPersonal() {
        return (this.comPersonal!= null);
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

    public boolean isSetComDate() {
        return (this.comDate!= null);
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

    public boolean isSetComAmount() {
        return (this.comAmount!= null);
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

    public boolean isSetComComment() {
        return (this.comComment!= null);
    }

}

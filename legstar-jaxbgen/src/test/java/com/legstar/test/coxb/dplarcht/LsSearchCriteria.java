
package com.legstar.test.coxb.dplarcht;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for LsSearchCriteria complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsSearchCriteria">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsStartwith">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="LsStartwithLen">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *               &lt;totalDigits value="9"/>
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
@XmlType(name = "LsSearchCriteria", propOrder = {
    "lsStartwith",
    "lsStartwithLen"
})
public class LsSearchCriteria
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "LsStartwith", required = true)
    @CobolElement(cobolName = "LS-STARTWITH", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 15, picture = "X(8)", srceLine = 90)
    protected String lsStartwith;
    @XmlElement(name = "LsStartwithLen")
    @CobolElement(cobolName = "LS-STARTWITH-LEN", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 15, isSigned = false, totalDigits = 9, picture = "9(9)", usage = "PACKED-DECIMAL", srceLine = 91)
    protected long lsStartwithLen;

    /**
     * Gets the value of the lsStartwith property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLsStartwith() {
        return lsStartwith;
    }

    /**
     * Sets the value of the lsStartwith property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLsStartwith(String value) {
        this.lsStartwith = value;
    }

    public boolean isSetLsStartwith() {
        return (this.lsStartwith!= null);
    }

    /**
     * Gets the value of the lsStartwithLen property.
     * 
     */
    public long getLsStartwithLen() {
        return lsStartwithLen;
    }

    /**
     * Sets the value of the lsStartwithLen property.
     * 
     */
    public void setLsStartwithLen(long value) {
        this.lsStartwithLen = value;
    }

    public boolean isSetLsStartwithLen() {
        return true;
    }

}

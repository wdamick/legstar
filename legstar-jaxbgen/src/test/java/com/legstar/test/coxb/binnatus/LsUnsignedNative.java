
package com.legstar.test.coxb.binnatus;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for LsUnsignedNative complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsUnsignedNative">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsHalfwords" type="{http://legstar.com/test/coxb/binnatus}LsHalfwords"/>
 *         &lt;element name="LsFullwords" type="{http://legstar.com/test/coxb/binnatus}LsFullwords"/>
 *         &lt;element name="LsDoublewords" type="{http://legstar.com/test/coxb/binnatus}LsDoublewords"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsUnsignedNative", propOrder = {
    "lsHalfwords",
    "lsFullwords",
    "lsDoublewords"
})
public class LsUnsignedNative
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "LsHalfwords", required = true)
    @CobolElement(cobolName = "LS-HALFWORDS", type = CobolType.GROUP_ITEM, levelNumber = 10, srceLine = 69)
    protected LsHalfwords lsHalfwords;
    @XmlElement(name = "LsFullwords", required = true)
    @CobolElement(cobolName = "LS-FULLWORDS", type = CobolType.GROUP_ITEM, levelNumber = 10, srceLine = 74)
    protected LsFullwords lsFullwords;
    @XmlElement(name = "LsDoublewords", required = true)
    @CobolElement(cobolName = "LS-DOUBLEWORDS", type = CobolType.GROUP_ITEM, levelNumber = 10, srceLine = 79)
    protected LsDoublewords lsDoublewords;

    /**
     * Gets the value of the lsHalfwords property.
     * 
     * @return
     *     possible object is
     *     {@link LsHalfwords }
     *     
     */
    public LsHalfwords getLsHalfwords() {
        return lsHalfwords;
    }

    /**
     * Sets the value of the lsHalfwords property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsHalfwords }
     *     
     */
    public void setLsHalfwords(LsHalfwords value) {
        this.lsHalfwords = value;
    }

    public boolean isSetLsHalfwords() {
        return (this.lsHalfwords!= null);
    }

    /**
     * Gets the value of the lsFullwords property.
     * 
     * @return
     *     possible object is
     *     {@link LsFullwords }
     *     
     */
    public LsFullwords getLsFullwords() {
        return lsFullwords;
    }

    /**
     * Sets the value of the lsFullwords property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsFullwords }
     *     
     */
    public void setLsFullwords(LsFullwords value) {
        this.lsFullwords = value;
    }

    public boolean isSetLsFullwords() {
        return (this.lsFullwords!= null);
    }

    /**
     * Gets the value of the lsDoublewords property.
     * 
     * @return
     *     possible object is
     *     {@link LsDoublewords }
     *     
     */
    public LsDoublewords getLsDoublewords() {
        return lsDoublewords;
    }

    /**
     * Sets the value of the lsDoublewords property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsDoublewords }
     *     
     */
    public void setLsDoublewords(LsDoublewords value) {
        this.lsDoublewords = value;
    }

    public boolean isSetLsDoublewords() {
        return (this.lsDoublewords!= null);
    }

}

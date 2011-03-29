
package com.legstar.test.coxb.binpkdus;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for LsUnsignedPackedDecimal complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsUnsignedPackedDecimal">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsCompat" type="{http://legstar.com/test/coxb/binpkdus}LsCompat"/>
 *         &lt;element name="LsExtend" type="{http://legstar.com/test/coxb/binpkdus}LsExtend"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsUnsignedPackedDecimal", propOrder = {
    "lsCompat",
    "lsExtend"
})
public class LsUnsignedPackedDecimal
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "LsCompat", required = true)
    @CobolElement(cobolName = "LS-COMPAT", type = CobolType.GROUP_ITEM, levelNumber = 10, srceLine = 57)
    protected LsCompat lsCompat;
    @XmlElement(name = "LsExtend", required = true)
    @CobolElement(cobolName = "LS-EXTEND", type = CobolType.GROUP_ITEM, levelNumber = 10, srceLine = 63)
    protected LsExtend lsExtend;

    /**
     * Gets the value of the lsCompat property.
     * 
     * @return
     *     possible object is
     *     {@link LsCompat }
     *     
     */
    public LsCompat getLsCompat() {
        return lsCompat;
    }

    /**
     * Sets the value of the lsCompat property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsCompat }
     *     
     */
    public void setLsCompat(LsCompat value) {
        this.lsCompat = value;
    }

    public boolean isSetLsCompat() {
        return (this.lsCompat!= null);
    }

    /**
     * Gets the value of the lsExtend property.
     * 
     * @return
     *     possible object is
     *     {@link LsExtend }
     *     
     */
    public LsExtend getLsExtend() {
        return lsExtend;
    }

    /**
     * Sets the value of the lsExtend property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsExtend }
     *     
     */
    public void setLsExtend(LsExtend value) {
        this.lsExtend = value;
    }

    public boolean isSetLsExtend() {
        return (this.lsExtend!= null);
    }

}


package com.legstar.test.coxb.lsfileac;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for WQueryLimit complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WQueryLimit">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="WMaxItemsRead">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *               &lt;totalDigits value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WMaxElapseTime">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *               &lt;totalDigits value="8"/>
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
@XmlType(name = "WQueryLimit", propOrder = {
    "wMaxItemsRead",
    "wMaxElapseTime"
})
public class WQueryLimit
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "WMaxItemsRead")
    @CobolElement(cobolName = "W-MAX-ITEMS-READ", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 8, picture = "9(8)", usage = "PACKED-DECIMAL", value = "200", srceLine = 37)
    protected long wMaxItemsRead = 200L;
    @XmlElement(name = "WMaxElapseTime")
    @CobolElement(cobolName = "W-MAX-ELAPSE-TIME", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 8, picture = "9(8)", usage = "PACKED-DECIMAL", value = "10000", srceLine = 38)
    protected long wMaxElapseTime = 10000L;

    /**
     * Gets the value of the wMaxItemsRead property.
     * 
     */
    public long getWMaxItemsRead() {
        return wMaxItemsRead;
    }

    /**
     * Sets the value of the wMaxItemsRead property.
     * 
     */
    public void setWMaxItemsRead(long value) {
        this.wMaxItemsRead = value;
    }

    public boolean isSetWMaxItemsRead() {
        return true;
    }

    /**
     * Gets the value of the wMaxElapseTime property.
     * 
     */
    public long getWMaxElapseTime() {
        return wMaxElapseTime;
    }

    /**
     * Sets the value of the wMaxElapseTime property.
     * 
     */
    public void setWMaxElapseTime(long value) {
        this.wMaxElapseTime = value;
    }

    public boolean isSetWMaxElapseTime() {
        return true;
    }

}

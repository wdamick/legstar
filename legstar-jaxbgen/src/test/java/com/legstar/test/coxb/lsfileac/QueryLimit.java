
package com.legstar.test.coxb.lsfileac;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for QueryLimit complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="QueryLimit">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="MaxItemsRead">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *               &lt;totalDigits value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="MaxElapseTime">
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
@XmlType(name = "QueryLimit", propOrder = {
    "maxItemsRead",
    "maxElapseTime"
})
public class QueryLimit
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "MaxItemsRead")
    @CobolElement(cobolName = "MAX-ITEMS-READ", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 8, picture = "9(8)", usage = "PACKED-DECIMAL", srceLine = 97)
    protected long maxItemsRead;
    @XmlElement(name = "MaxElapseTime")
    @CobolElement(cobolName = "MAX-ELAPSE-TIME", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 8, picture = "9(8)", usage = "PACKED-DECIMAL", srceLine = 98)
    protected long maxElapseTime;

    /**
     * Gets the value of the maxItemsRead property.
     * 
     */
    public long getMaxItemsRead() {
        return maxItemsRead;
    }

    /**
     * Sets the value of the maxItemsRead property.
     * 
     */
    public void setMaxItemsRead(long value) {
        this.maxItemsRead = value;
    }

    public boolean isSetMaxItemsRead() {
        return true;
    }

    /**
     * Gets the value of the maxElapseTime property.
     * 
     */
    public long getMaxElapseTime() {
        return maxElapseTime;
    }

    /**
     * Sets the value of the maxElapseTime property.
     * 
     */
    public void setMaxElapseTime(long value) {
        this.maxElapseTime = value;
    }

    public boolean isSetMaxElapseTime() {
        return true;
    }

}

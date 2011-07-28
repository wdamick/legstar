
package com.legstar.test.coxb.lsfileac;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


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
 *         &lt;element name="MaxItemsRead" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="MaxElapseTime" type="{http://www.w3.org/2001/XMLSchema}long"/>
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
public class QueryLimit {

    @XmlElement(name = "MaxItemsRead")
    protected long maxItemsRead;
    @XmlElement(name = "MaxElapseTime")
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

}

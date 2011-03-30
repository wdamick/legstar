
package com.legstar.test.coxb.lsfileal;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for Filler65 complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Filler65">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ReplyItemscount" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="ReplyItem" type="{http://legstar.com/test/coxb/lsfileal}ReplyItem" maxOccurs="unbounded"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Filler65", propOrder = {
    "replyItemscount",
    "replyItem"
})
public class Filler65 {

    @XmlElement(name = "ReplyItemscount")
    protected long replyItemscount;
    @XmlElement(name = "ReplyItem", required = true)
    protected List<ReplyItem> replyItem;

    /**
     * Gets the value of the replyItemscount property.
     * 
     */
    public long getReplyItemscount() {
        return replyItemscount;
    }

    /**
     * Sets the value of the replyItemscount property.
     * 
     */
    public void setReplyItemscount(long value) {
        this.replyItemscount = value;
    }

    /**
     * Gets the value of the replyItem property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the replyItem property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getReplyItem().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ReplyItem }
     * 
     * 
     */
    public List<ReplyItem> getReplyItem() {
        if (replyItem == null) {
            replyItem = new ArrayList<ReplyItem>();
        }
        return this.replyItem;
    }

}

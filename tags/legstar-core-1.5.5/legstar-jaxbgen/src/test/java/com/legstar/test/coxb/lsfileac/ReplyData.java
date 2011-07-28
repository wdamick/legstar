
package com.legstar.test.coxb.lsfileac;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for ReplyData complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ReplyData">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ReplyItemscount">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *               &lt;totalDigits value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="ReplyItem" type="{http://legstar.com/test/coxb/lsfileac}ReplyItem" maxOccurs="100"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ReplyData", propOrder = {
    "replyItemscount",
    "replyItem"
})
public class ReplyData
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "ReplyItemscount")
    @CobolElement(cobolName = "REPLY-ITEMSCOUNT", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 8, isODOObject = true, picture = "9(8)", usage = "PACKED-DECIMAL", srceLine = 64)
    protected long replyItemscount;
    @XmlElement(name = "ReplyItem", required = true)
    @CobolElement(cobolName = "REPLY-ITEM", type = CobolType.GROUP_ITEM, levelNumber = 5, minOccurs = 1, maxOccurs = 100, dependingOn = "REPLY-ITEMSCOUNT", srceLine = 65)
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

    public boolean isSetReplyItemscount() {
        return true;
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

    public boolean isSetReplyItem() {
        return ((this.replyItem!= null)&&(!this.replyItem.isEmpty()));
    }

    public void unsetReplyItem() {
        this.replyItem = null;
    }

}

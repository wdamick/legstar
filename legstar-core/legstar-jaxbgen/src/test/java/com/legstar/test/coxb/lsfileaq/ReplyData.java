
package com.legstar.test.coxb.lsfileaq;

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
 *         &lt;element name="ReplyCount">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *               &lt;totalDigits value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Customer" type="{http://legstar.com/test/coxb/lsfileaq}Customer" maxOccurs="100"/>
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
    "replyCount",
    "customer"
})
public class ReplyData
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "ReplyCount")
    @CobolElement(cobolName = "REPLY-COUNT", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 10, isSigned = false, totalDigits = 8, isODOObject = true, picture = "9(8)", usage = "PACKED-DECIMAL", srceLine = 41)
    protected long replyCount;
    @XmlElement(name = "Customer", required = true)
    @CobolElement(cobolName = "CUSTOMER", type = CobolType.GROUP_ITEM, levelNumber = 10, minOccurs = 1, maxOccurs = 100, dependingOn = "REPLY-COUNT", srceLine = 42)
    protected List<Customer> customer;

    /**
     * Gets the value of the replyCount property.
     * 
     */
    public long getReplyCount() {
        return replyCount;
    }

    /**
     * Sets the value of the replyCount property.
     * 
     */
    public void setReplyCount(long value) {
        this.replyCount = value;
    }

    public boolean isSetReplyCount() {
        return true;
    }

    /**
     * Gets the value of the customer property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the customer property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getCustomer().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Customer }
     * 
     * 
     */
    public List<Customer> getCustomer() {
        if (customer == null) {
            customer = new ArrayList<Customer>();
        }
        return this.customer;
    }

    public boolean isSetCustomer() {
        return ((this.customer!= null)&&(!this.customer.isEmpty()));
    }

    public void unsetCustomer() {
        this.customer = null;
    }

}

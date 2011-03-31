
package com.legstar.test.coxb.dplarcht;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for LsReply complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsReply">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsReplyType">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *               &lt;totalDigits value="4"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="LsReplyData" type="{http://legstar.com/test/coxb/dplarcht}LsReplyData"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsReply", propOrder = {
    "lsReplyType",
    "lsReplyData"
})
public class LsReply
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "LsReplyType")
    @CobolElement(cobolName = "LS-REPLY-TYPE", type = CobolType.BINARY_ITEM, levelNumber = 10, isSigned = false, totalDigits = 4, picture = "9(4)", usage = "BINARY", srceLine = 94)
    protected int lsReplyType;
    @XmlElement(name = "LsReplyData", required = true)
    @CobolElement(cobolName = "LS-REPLY-DATA", type = CobolType.GROUP_ITEM, levelNumber = 10, srceLine = 98)
    protected LsReplyData lsReplyData;

    /**
     * Gets the value of the lsReplyType property.
     * 
     */
    public int getLsReplyType() {
        return lsReplyType;
    }

    /**
     * Sets the value of the lsReplyType property.
     * 
     */
    public void setLsReplyType(int value) {
        this.lsReplyType = value;
    }

    public boolean isSetLsReplyType() {
        return true;
    }

    /**
     * Gets the value of the lsReplyData property.
     * 
     * @return
     *     possible object is
     *     {@link LsReplyData }
     *     
     */
    public LsReplyData getLsReplyData() {
        return lsReplyData;
    }

    /**
     * Sets the value of the lsReplyData property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsReplyData }
     *     
     */
    public void setLsReplyData(LsReplyData value) {
        this.lsReplyData = value;
    }

    public boolean isSetLsReplyData() {
        return (this.lsReplyData!= null);
    }

}

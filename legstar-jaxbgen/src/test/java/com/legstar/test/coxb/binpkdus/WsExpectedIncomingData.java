
package com.legstar.test.coxb.binpkdus;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for WsExpectedIncomingData complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WsExpectedIncomingData">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="WsUnsignedPackedDecimal" type="{http://legstar.com/test/coxb/binpkdus}WsUnsignedPackedDecimal"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "WsExpectedIncomingData", propOrder = {
    "wsUnsignedPackedDecimal"
})
public class WsExpectedIncomingData
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "WsUnsignedPackedDecimal", required = true)
    @CobolElement(cobolName = "WS-UNSIGNED-PACKED-DECIMAL", type = CobolType.GROUP_ITEM, levelNumber = 5, srceLine = 30)
    protected WsUnsignedPackedDecimal wsUnsignedPackedDecimal;

    /**
     * Gets the value of the wsUnsignedPackedDecimal property.
     * 
     * @return
     *     possible object is
     *     {@link WsUnsignedPackedDecimal }
     *     
     */
    public WsUnsignedPackedDecimal getWsUnsignedPackedDecimal() {
        return wsUnsignedPackedDecimal;
    }

    /**
     * Sets the value of the wsUnsignedPackedDecimal property.
     * 
     * @param value
     *     allowed object is
     *     {@link WsUnsignedPackedDecimal }
     *     
     */
    public void setWsUnsignedPackedDecimal(WsUnsignedPackedDecimal value) {
        this.wsUnsignedPackedDecimal = value;
    }

    public boolean isSetWsUnsignedPackedDecimal() {
        return (this.wsUnsignedPackedDecimal!= null);
    }

}

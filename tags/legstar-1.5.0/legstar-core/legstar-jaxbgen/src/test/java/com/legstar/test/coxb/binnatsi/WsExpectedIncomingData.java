
package com.legstar.test.coxb.binnatsi;

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
 *         &lt;element name="WsUnsignedNative" type="{http://legstar.com/test/coxb/binnatsi}WsUnsignedNative"/>
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
    "wsUnsignedNative"
})
public class WsExpectedIncomingData
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "WsUnsignedNative", required = true)
    @CobolElement(cobolName = "WS-UNSIGNED-NATIVE", type = CobolType.GROUP_ITEM, levelNumber = 5, srceLine = 30)
    protected WsUnsignedNative wsUnsignedNative;

    /**
     * Gets the value of the wsUnsignedNative property.
     * 
     * @return
     *     possible object is
     *     {@link WsUnsignedNative }
     *     
     */
    public WsUnsignedNative getWsUnsignedNative() {
        return wsUnsignedNative;
    }

    /**
     * Sets the value of the wsUnsignedNative property.
     * 
     * @param value
     *     allowed object is
     *     {@link WsUnsignedNative }
     *     
     */
    public void setWsUnsignedNative(WsUnsignedNative value) {
        this.wsUnsignedNative = value;
    }

    public boolean isSetWsUnsignedNative() {
        return (this.wsUnsignedNative!= null);
    }

}


package com.legstar.test.coxb.binnatsi;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for WsUnsignedNative complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WsUnsignedNative">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="WsHalfwords" type="{http://legstar.com/test/coxb/binnatsi}WsHalfwords"/>
 *         &lt;element name="WsFullwords" type="{http://legstar.com/test/coxb/binnatsi}WsFullwords"/>
 *         &lt;element name="WsDoublewords" type="{http://legstar.com/test/coxb/binnatsi}WsDoublewords"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "WsUnsignedNative", propOrder = {
    "wsHalfwords",
    "wsFullwords",
    "wsDoublewords"
})
public class WsUnsignedNative
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "WsHalfwords", required = true)
    @CobolElement(cobolName = "WS-HALFWORDS", type = CobolType.GROUP_ITEM, levelNumber = 10, srceLine = 31)
    protected WsHalfwords wsHalfwords;
    @XmlElement(name = "WsFullwords", required = true)
    @CobolElement(cobolName = "WS-FULLWORDS", type = CobolType.GROUP_ITEM, levelNumber = 10, srceLine = 36)
    protected WsFullwords wsFullwords;
    @XmlElement(name = "WsDoublewords", required = true)
    @CobolElement(cobolName = "WS-DOUBLEWORDS", type = CobolType.GROUP_ITEM, levelNumber = 10, srceLine = 41)
    protected WsDoublewords wsDoublewords;

    /**
     * Gets the value of the wsHalfwords property.
     * 
     * @return
     *     possible object is
     *     {@link WsHalfwords }
     *     
     */
    public WsHalfwords getWsHalfwords() {
        return wsHalfwords;
    }

    /**
     * Sets the value of the wsHalfwords property.
     * 
     * @param value
     *     allowed object is
     *     {@link WsHalfwords }
     *     
     */
    public void setWsHalfwords(WsHalfwords value) {
        this.wsHalfwords = value;
    }

    public boolean isSetWsHalfwords() {
        return (this.wsHalfwords!= null);
    }

    /**
     * Gets the value of the wsFullwords property.
     * 
     * @return
     *     possible object is
     *     {@link WsFullwords }
     *     
     */
    public WsFullwords getWsFullwords() {
        return wsFullwords;
    }

    /**
     * Sets the value of the wsFullwords property.
     * 
     * @param value
     *     allowed object is
     *     {@link WsFullwords }
     *     
     */
    public void setWsFullwords(WsFullwords value) {
        this.wsFullwords = value;
    }

    public boolean isSetWsFullwords() {
        return (this.wsFullwords!= null);
    }

    /**
     * Gets the value of the wsDoublewords property.
     * 
     * @return
     *     possible object is
     *     {@link WsDoublewords }
     *     
     */
    public WsDoublewords getWsDoublewords() {
        return wsDoublewords;
    }

    /**
     * Sets the value of the wsDoublewords property.
     * 
     * @param value
     *     allowed object is
     *     {@link WsDoublewords }
     *     
     */
    public void setWsDoublewords(WsDoublewords value) {
        this.wsDoublewords = value;
    }

    public boolean isSetWsDoublewords() {
        return (this.wsDoublewords!= null);
    }

}

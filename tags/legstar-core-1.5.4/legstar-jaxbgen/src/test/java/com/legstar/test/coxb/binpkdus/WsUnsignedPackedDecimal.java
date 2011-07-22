
package com.legstar.test.coxb.binpkdus;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for WsUnsignedPackedDecimal complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WsUnsignedPackedDecimal">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="WsCompat" type="{http://legstar.com/test/coxb/binpkdus}WsCompat"/>
 *         &lt;element name="WsExtend" type="{http://legstar.com/test/coxb/binpkdus}WsExtend"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "WsUnsignedPackedDecimal", propOrder = {
    "wsCompat",
    "wsExtend"
})
public class WsUnsignedPackedDecimal
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "WsCompat", required = true)
    @CobolElement(cobolName = "WS-COMPAT", type = CobolType.GROUP_ITEM, levelNumber = 10, srceLine = 31)
    protected WsCompat wsCompat;
    @XmlElement(name = "WsExtend", required = true)
    @CobolElement(cobolName = "WS-EXTEND", type = CobolType.GROUP_ITEM, levelNumber = 10, srceLine = 39)
    protected WsExtend wsExtend;

    /**
     * Gets the value of the wsCompat property.
     * 
     * @return
     *     possible object is
     *     {@link WsCompat }
     *     
     */
    public WsCompat getWsCompat() {
        return wsCompat;
    }

    /**
     * Sets the value of the wsCompat property.
     * 
     * @param value
     *     allowed object is
     *     {@link WsCompat }
     *     
     */
    public void setWsCompat(WsCompat value) {
        this.wsCompat = value;
    }

    public boolean isSetWsCompat() {
        return (this.wsCompat!= null);
    }

    /**
     * Gets the value of the wsExtend property.
     * 
     * @return
     *     possible object is
     *     {@link WsExtend }
     *     
     */
    public WsExtend getWsExtend() {
        return wsExtend;
    }

    /**
     * Sets the value of the wsExtend property.
     * 
     * @param value
     *     allowed object is
     *     {@link WsExtend }
     *     
     */
    public void setWsExtend(WsExtend value) {
        this.wsExtend = value;
    }

    public boolean isSetWsExtend() {
        return (this.wsExtend!= null);
    }

}


package com.legstar.test.coxb.binnatsi;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for WsDoublewords complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WsDoublewords">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;choice>
 *           &lt;element name="WsPs9X18MinB" type="{http://legstar.com/test/coxb/binnatsi}WsPs9X18MinB"/>
 *           &lt;element name="WsPs9X18Min">
 *             &lt;simpleType>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}long">
 *               &lt;/restriction>
 *             &lt;/simpleType>
 *           &lt;/element>
 *         &lt;/choice>
 *         &lt;element name="WsPs9X18Low">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}long">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;choice>
 *           &lt;element name="WsPs9X18HighB" type="{http://legstar.com/test/coxb/binnatsi}WsPs9X18HighB"/>
 *           &lt;element name="WsPs9X18High">
 *             &lt;simpleType>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}long">
 *               &lt;/restriction>
 *             &lt;/simpleType>
 *           &lt;/element>
 *         &lt;/choice>
 *         &lt;element name="WsPs9X18MaxB" type="{http://legstar.com/test/coxb/binnatsi}WsPs9X18MaxB"/>
 *         &lt;element name="WsPs9X18Max">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}long">
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
@XmlType(name = "WsDoublewords", propOrder = {
    "wsPs9X18MinB",
    "wsPs9X18Min",
    "wsPs9X18Low",
    "wsPs9X18HighB",
    "wsPs9X18High",
    "wsPs9X18MaxB",
    "wsPs9X18Max"
})
public class WsDoublewords
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "WsPs9X18MinB")
    @CobolElement(cobolName = "WS-PS9X18-MIN-B", type = CobolType.GROUP_ITEM, levelNumber = 15, isRedefined = true, srceLine = 42)
    protected WsPs9X18MinB wsPs9X18MinB;
    @XmlElement(name = "WsPs9X18Min")
    @CobolElement(cobolName = "WS-PS9X18-MIN", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 15, isSigned = true, totalDigits = 18, redefines = "WS-PS9X18-MIN-B", picture = "S9(18)", usage = "COMP-5", srceLine = 45)
    protected Long wsPs9X18Min;
    @XmlElement(name = "WsPs9X18Low")
    @CobolElement(cobolName = "WS-PS9X18-LOW", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 15, isSigned = true, totalDigits = 18, picture = "S9(18)", usage = "COMP-5", value = "-4294967294", srceLine = 47)
    protected long wsPs9X18Low = -4294967294L;
    @XmlElement(name = "WsPs9X18HighB")
    @CobolElement(cobolName = "WS-PS9X18-HIGH-B", type = CobolType.GROUP_ITEM, levelNumber = 15, isRedefined = true, srceLine = 48)
    protected WsPs9X18HighB wsPs9X18HighB;
    @XmlElement(name = "WsPs9X18High")
    @CobolElement(cobolName = "WS-PS9X18-HIGH", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 15, isSigned = true, totalDigits = 18, redefines = "WS-PS9X18-HIGH-B", picture = "S9(18)", usage = "COMP-5", srceLine = 51)
    protected Long wsPs9X18High;
    @XmlElement(name = "WsPs9X18MaxB", required = true)
    @CobolElement(cobolName = "WS-PS9X18-MAX-B", type = CobolType.GROUP_ITEM, levelNumber = 15, isRedefined = true, srceLine = 53)
    protected WsPs9X18MaxB wsPs9X18MaxB;
    @XmlElement(name = "WsPs9X18Max")
    @CobolElement(cobolName = "WS-PS9X18-MAX", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 15, isSigned = true, totalDigits = 18, redefines = "WS-PS9X18-MAX-B", picture = "S9(18)", usage = "COMP-5", srceLine = 56)
    protected long wsPs9X18Max;

    /**
     * Gets the value of the wsPs9X18MinB property.
     * 
     * @return
     *     possible object is
     *     {@link WsPs9X18MinB }
     *     
     */
    public WsPs9X18MinB getWsPs9X18MinB() {
        return wsPs9X18MinB;
    }

    /**
     * Sets the value of the wsPs9X18MinB property.
     * 
     * @param value
     *     allowed object is
     *     {@link WsPs9X18MinB }
     *     
     */
    public void setWsPs9X18MinB(WsPs9X18MinB value) {
        this.wsPs9X18MinB = value;
    }

    public boolean isSetWsPs9X18MinB() {
        return (this.wsPs9X18MinB!= null);
    }

    /**
     * Gets the value of the wsPs9X18Min property.
     * 
     * @return
     *     possible object is
     *     {@link Long }
     *     
     */
    public Long getWsPs9X18Min() {
        return wsPs9X18Min;
    }

    /**
     * Sets the value of the wsPs9X18Min property.
     * 
     * @param value
     *     allowed object is
     *     {@link Long }
     *     
     */
    public void setWsPs9X18Min(Long value) {
        this.wsPs9X18Min = value;
    }

    public boolean isSetWsPs9X18Min() {
        return (this.wsPs9X18Min!= null);
    }

    /**
     * Gets the value of the wsPs9X18Low property.
     * 
     */
    public long getWsPs9X18Low() {
        return wsPs9X18Low;
    }

    /**
     * Sets the value of the wsPs9X18Low property.
     * 
     */
    public void setWsPs9X18Low(long value) {
        this.wsPs9X18Low = value;
    }

    public boolean isSetWsPs9X18Low() {
        return true;
    }

    /**
     * Gets the value of the wsPs9X18HighB property.
     * 
     * @return
     *     possible object is
     *     {@link WsPs9X18HighB }
     *     
     */
    public WsPs9X18HighB getWsPs9X18HighB() {
        return wsPs9X18HighB;
    }

    /**
     * Sets the value of the wsPs9X18HighB property.
     * 
     * @param value
     *     allowed object is
     *     {@link WsPs9X18HighB }
     *     
     */
    public void setWsPs9X18HighB(WsPs9X18HighB value) {
        this.wsPs9X18HighB = value;
    }

    public boolean isSetWsPs9X18HighB() {
        return (this.wsPs9X18HighB!= null);
    }

    /**
     * Gets the value of the wsPs9X18High property.
     * 
     * @return
     *     possible object is
     *     {@link Long }
     *     
     */
    public Long getWsPs9X18High() {
        return wsPs9X18High;
    }

    /**
     * Sets the value of the wsPs9X18High property.
     * 
     * @param value
     *     allowed object is
     *     {@link Long }
     *     
     */
    public void setWsPs9X18High(Long value) {
        this.wsPs9X18High = value;
    }

    public boolean isSetWsPs9X18High() {
        return (this.wsPs9X18High!= null);
    }

    /**
     * Gets the value of the wsPs9X18MaxB property.
     * 
     * @return
     *     possible object is
     *     {@link WsPs9X18MaxB }
     *     
     */
    public WsPs9X18MaxB getWsPs9X18MaxB() {
        return wsPs9X18MaxB;
    }

    /**
     * Sets the value of the wsPs9X18MaxB property.
     * 
     * @param value
     *     allowed object is
     *     {@link WsPs9X18MaxB }
     *     
     */
    public void setWsPs9X18MaxB(WsPs9X18MaxB value) {
        this.wsPs9X18MaxB = value;
    }

    public boolean isSetWsPs9X18MaxB() {
        return (this.wsPs9X18MaxB!= null);
    }

    /**
     * Gets the value of the wsPs9X18Max property.
     * 
     */
    public long getWsPs9X18Max() {
        return wsPs9X18Max;
    }

    /**
     * Sets the value of the wsPs9X18Max property.
     * 
     */
    public void setWsPs9X18Max(long value) {
        this.wsPs9X18Max = value;
    }

    public boolean isSetWsPs9X18Max() {
        return true;
    }

}

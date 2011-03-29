
package com.legstar.test.coxb.binpkdus;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for WsCompat complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WsCompat">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="WsP9X1Null">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *               &lt;totalDigits value="1"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsP9X1">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *               &lt;totalDigits value="1"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsP9X2">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *               &lt;totalDigits value="2"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsP9X7">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *               &lt;totalDigits value="7"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsP9X18">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedLong">
 *               &lt;maxInclusive value="999999999999999999"/>
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
@XmlType(name = "WsCompat", propOrder = {
    "wsP9X1Null",
    "wsP9X1",
    "wsP9X2",
    "wsP9X7",
    "wsP9X18"
})
public class WsCompat
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "WsP9X1Null")
    @CobolElement(cobolName = "WS-P9X1-NULL", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 15, isSigned = false, totalDigits = 1, picture = "9(1)", usage = "PACKED-DECIMAL", value = "0", srceLine = 32)
    protected int wsP9X1Null = 0;
    @XmlElement(name = "WsP9X1")
    @CobolElement(cobolName = "WS-P9X1", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 15, isSigned = false, totalDigits = 1, picture = "9(1)", usage = "PACKED-DECIMAL", value = "3", srceLine = 33)
    protected int wsP9X1 = 3;
    @XmlElement(name = "WsP9X2")
    @CobolElement(cobolName = "WS-P9X2", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 15, isSigned = false, totalDigits = 2, picture = "9(2)", usage = "PACKED-DECIMAL", value = "12", srceLine = 34)
    protected int wsP9X2 = 12;
    @XmlElement(name = "WsP9X7")
    @CobolElement(cobolName = "WS-P9X7", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 15, isSigned = false, totalDigits = 7, picture = "9(7)", usage = "PACKED-DECIMAL", value = "32769", srceLine = 35)
    protected long wsP9X7 = 32769L;
    @XmlElement(name = "WsP9X18")
    @CobolElement(cobolName = "WS-P9X18", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 15, isSigned = false, totalDigits = 18, picture = "9(18)", usage = "PACKED-DECIMAL", value = "123456789012345678", srceLine = 37)
    protected long wsP9X18 = 123456789012345678L;

    /**
     * Gets the value of the wsP9X1Null property.
     * 
     */
    public int getWsP9X1Null() {
        return wsP9X1Null;
    }

    /**
     * Sets the value of the wsP9X1Null property.
     * 
     */
    public void setWsP9X1Null(int value) {
        this.wsP9X1Null = value;
    }

    public boolean isSetWsP9X1Null() {
        return true;
    }

    /**
     * Gets the value of the wsP9X1 property.
     * 
     */
    public int getWsP9X1() {
        return wsP9X1;
    }

    /**
     * Sets the value of the wsP9X1 property.
     * 
     */
    public void setWsP9X1(int value) {
        this.wsP9X1 = value;
    }

    public boolean isSetWsP9X1() {
        return true;
    }

    /**
     * Gets the value of the wsP9X2 property.
     * 
     */
    public int getWsP9X2() {
        return wsP9X2;
    }

    /**
     * Sets the value of the wsP9X2 property.
     * 
     */
    public void setWsP9X2(int value) {
        this.wsP9X2 = value;
    }

    public boolean isSetWsP9X2() {
        return true;
    }

    /**
     * Gets the value of the wsP9X7 property.
     * 
     */
    public long getWsP9X7() {
        return wsP9X7;
    }

    /**
     * Sets the value of the wsP9X7 property.
     * 
     */
    public void setWsP9X7(long value) {
        this.wsP9X7 = value;
    }

    public boolean isSetWsP9X7() {
        return true;
    }

    /**
     * Gets the value of the wsP9X18 property.
     * 
     */
    public long getWsP9X18() {
        return wsP9X18;
    }

    /**
     * Sets the value of the wsP9X18 property.
     * 
     */
    public void setWsP9X18(long value) {
        this.wsP9X18 = value;
    }

    public boolean isSetWsP9X18() {
        return true;
    }

}

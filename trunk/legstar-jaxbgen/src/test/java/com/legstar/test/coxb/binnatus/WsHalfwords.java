
package com.legstar.test.coxb.binnatus;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for WsHalfwords complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WsHalfwords">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="WsP9X4Min">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsP9X4Low">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsP9X4High">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsP9X4Max">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
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
@XmlType(name = "WsHalfwords", propOrder = {
    "wsP9X4Min",
    "wsP9X4Low",
    "wsP9X4High",
    "wsP9X4Max"
})
public class WsHalfwords
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "WsP9X4Min")
    @CobolElement(cobolName = "WS-P9X4-MIN", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 15, isSigned = false, totalDigits = 4, picture = "9(4)", usage = "COMP-5", value = "0", srceLine = 32)
    protected int wsP9X4Min = 0;
    @XmlElement(name = "WsP9X4Low")
    @CobolElement(cobolName = "WS-P9X4-LOW", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 15, isSigned = false, totalDigits = 4, picture = "9(4)", usage = "COMP-5", value = "127", srceLine = 33)
    protected int wsP9X4Low = 127;
    @XmlElement(name = "WsP9X4High")
    @CobolElement(cobolName = "WS-P9X4-HIGH", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 15, isSigned = false, totalDigits = 4, picture = "9(4)", usage = "COMP-5", value = "32769", srceLine = 34)
    protected int wsP9X4High = 32769;
    @XmlElement(name = "WsP9X4Max")
    @CobolElement(cobolName = "WS-P9X4-MAX", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 15, isSigned = false, totalDigits = 4, picture = "9(4)", usage = "COMP-5", value = "65535", srceLine = 35)
    protected int wsP9X4Max = 65535;

    /**
     * Gets the value of the wsP9X4Min property.
     * 
     */
    public int getWsP9X4Min() {
        return wsP9X4Min;
    }

    /**
     * Sets the value of the wsP9X4Min property.
     * 
     */
    public void setWsP9X4Min(int value) {
        this.wsP9X4Min = value;
    }

    public boolean isSetWsP9X4Min() {
        return true;
    }

    /**
     * Gets the value of the wsP9X4Low property.
     * 
     */
    public int getWsP9X4Low() {
        return wsP9X4Low;
    }

    /**
     * Sets the value of the wsP9X4Low property.
     * 
     */
    public void setWsP9X4Low(int value) {
        this.wsP9X4Low = value;
    }

    public boolean isSetWsP9X4Low() {
        return true;
    }

    /**
     * Gets the value of the wsP9X4High property.
     * 
     */
    public int getWsP9X4High() {
        return wsP9X4High;
    }

    /**
     * Sets the value of the wsP9X4High property.
     * 
     */
    public void setWsP9X4High(int value) {
        this.wsP9X4High = value;
    }

    public boolean isSetWsP9X4High() {
        return true;
    }

    /**
     * Gets the value of the wsP9X4Max property.
     * 
     */
    public int getWsP9X4Max() {
        return wsP9X4Max;
    }

    /**
     * Sets the value of the wsP9X4Max property.
     * 
     */
    public void setWsP9X4Max(int value) {
        this.wsP9X4Max = value;
    }

    public boolean isSetWsP9X4Max() {
        return true;
    }

}

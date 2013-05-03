
package com.legstar.test.coxb.binnatus;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for WsFullwords complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WsFullwords">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="WsP9X9Min">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsP9X9Low">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsP9X9High">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsP9X9Max">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
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
@XmlType(name = "WsFullwords", propOrder = {
    "wsP9X9Min",
    "wsP9X9Low",
    "wsP9X9High",
    "wsP9X9Max"
})
public class WsFullwords
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "WsP9X9Min")
    @CobolElement(cobolName = "WS-P9X9-MIN", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 15, isSigned = false, totalDigits = 9, picture = "9(9)", usage = "COMP-5", value = "0", srceLine = 37)
    protected long wsP9X9Min = 0L;
    @XmlElement(name = "WsP9X9Low")
    @CobolElement(cobolName = "WS-P9X9-LOW", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 15, isSigned = false, totalDigits = 9, picture = "9(9)", usage = "COMP-5", value = "65534", srceLine = 38)
    protected long wsP9X9Low = 65534L;
    @XmlElement(name = "WsP9X9High")
    @CobolElement(cobolName = "WS-P9X9-HIGH", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 15, isSigned = false, totalDigits = 9, picture = "9(9)", usage = "COMP-5", value = "2147483649", srceLine = 39)
    protected long wsP9X9High = 2147483649L;
    @XmlElement(name = "WsP9X9Max")
    @CobolElement(cobolName = "WS-P9X9-MAX", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 15, isSigned = false, totalDigits = 9, picture = "9(9)", usage = "COMP-5", value = "4294967295", srceLine = 40)
    protected long wsP9X9Max = 4294967295L;

    /**
     * Gets the value of the wsP9X9Min property.
     * 
     */
    public long getWsP9X9Min() {
        return wsP9X9Min;
    }

    /**
     * Sets the value of the wsP9X9Min property.
     * 
     */
    public void setWsP9X9Min(long value) {
        this.wsP9X9Min = value;
    }

    public boolean isSetWsP9X9Min() {
        return true;
    }

    /**
     * Gets the value of the wsP9X9Low property.
     * 
     */
    public long getWsP9X9Low() {
        return wsP9X9Low;
    }

    /**
     * Sets the value of the wsP9X9Low property.
     * 
     */
    public void setWsP9X9Low(long value) {
        this.wsP9X9Low = value;
    }

    public boolean isSetWsP9X9Low() {
        return true;
    }

    /**
     * Gets the value of the wsP9X9High property.
     * 
     */
    public long getWsP9X9High() {
        return wsP9X9High;
    }

    /**
     * Sets the value of the wsP9X9High property.
     * 
     */
    public void setWsP9X9High(long value) {
        this.wsP9X9High = value;
    }

    public boolean isSetWsP9X9High() {
        return true;
    }

    /**
     * Gets the value of the wsP9X9Max property.
     * 
     */
    public long getWsP9X9Max() {
        return wsP9X9Max;
    }

    /**
     * Sets the value of the wsP9X9Max property.
     * 
     */
    public void setWsP9X9Max(long value) {
        this.wsP9X9Max = value;
    }

    public boolean isSetWsP9X9Max() {
        return true;
    }

}


package com.legstar.test.coxb.binnatsi;

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
 *         &lt;element name="WsPs9X9Min">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}int">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsPs9X9Low">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}int">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsPs9X9High">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}int">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsPs9X9Max">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}int">
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
    "wsPs9X9Min",
    "wsPs9X9Low",
    "wsPs9X9High",
    "wsPs9X9Max"
})
public class WsFullwords
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "WsPs9X9Min")
    @CobolElement(cobolName = "WS-PS9X9-MIN", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 15, isSigned = true, totalDigits = 9, picture = "S9(9)", usage = "COMP-5", value = "-2147483648", srceLine = 37)
    protected int wsPs9X9Min = -2147483648;
    @XmlElement(name = "WsPs9X9Low")
    @CobolElement(cobolName = "WS-PS9X9-LOW", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 15, isSigned = true, totalDigits = 9, picture = "S9(9)", usage = "COMP-5", value = "-128", srceLine = 38)
    protected int wsPs9X9Low = -128;
    @XmlElement(name = "WsPs9X9High")
    @CobolElement(cobolName = "WS-PS9X9-HIGH", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 15, isSigned = true, totalDigits = 9, picture = "S9(9)", usage = "COMP-5", value = "+123456789", srceLine = 39)
    protected int wsPs9X9High = 123456789;
    @XmlElement(name = "WsPs9X9Max")
    @CobolElement(cobolName = "WS-PS9X9-MAX", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 15, isSigned = true, totalDigits = 9, picture = "S9(9)", usage = "COMP-5", value = "+2147483647", srceLine = 40)
    protected int wsPs9X9Max = 2147483647;

    /**
     * Gets the value of the wsPs9X9Min property.
     * 
     */
    public int getWsPs9X9Min() {
        return wsPs9X9Min;
    }

    /**
     * Sets the value of the wsPs9X9Min property.
     * 
     */
    public void setWsPs9X9Min(int value) {
        this.wsPs9X9Min = value;
    }

    public boolean isSetWsPs9X9Min() {
        return true;
    }

    /**
     * Gets the value of the wsPs9X9Low property.
     * 
     */
    public int getWsPs9X9Low() {
        return wsPs9X9Low;
    }

    /**
     * Sets the value of the wsPs9X9Low property.
     * 
     */
    public void setWsPs9X9Low(int value) {
        this.wsPs9X9Low = value;
    }

    public boolean isSetWsPs9X9Low() {
        return true;
    }

    /**
     * Gets the value of the wsPs9X9High property.
     * 
     */
    public int getWsPs9X9High() {
        return wsPs9X9High;
    }

    /**
     * Sets the value of the wsPs9X9High property.
     * 
     */
    public void setWsPs9X9High(int value) {
        this.wsPs9X9High = value;
    }

    public boolean isSetWsPs9X9High() {
        return true;
    }

    /**
     * Gets the value of the wsPs9X9Max property.
     * 
     */
    public int getWsPs9X9Max() {
        return wsPs9X9Max;
    }

    /**
     * Sets the value of the wsPs9X9Max property.
     * 
     */
    public void setWsPs9X9Max(int value) {
        this.wsPs9X9Max = value;
    }

    public boolean isSetWsPs9X9Max() {
        return true;
    }

}


package com.legstar.test.coxb.binnatsi;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for WsPs9X18MaxB complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WsPs9X18MaxB">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="Filler54">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler55">
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
@XmlType(name = "WsPs9X18MaxB", propOrder = {
    "filler54",
    "filler55"
})
public class WsPs9X18MaxB
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "Filler54")
    @CobolElement(cobolName = "FILLER", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 20, isSigned = false, totalDigits = 9, picture = "9(9)", usage = "COMP-5", value = "2147483647", srceLine = 54)
    protected long filler54 = 2147483647L;
    @XmlElement(name = "Filler55")
    @CobolElement(cobolName = "FILLER", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 20, isSigned = false, totalDigits = 9, picture = "9(9)", usage = "COMP-5", value = "4294967295", srceLine = 55)
    protected long filler55 = 4294967295L;

    /**
     * Gets the value of the filler54 property.
     * 
     */
    public long getFiller54() {
        return filler54;
    }

    /**
     * Sets the value of the filler54 property.
     * 
     */
    public void setFiller54(long value) {
        this.filler54 = value;
    }

    public boolean isSetFiller54() {
        return true;
    }

    /**
     * Gets the value of the filler55 property.
     * 
     */
    public long getFiller55() {
        return filler55;
    }

    /**
     * Sets the value of the filler55 property.
     * 
     */
    public void setFiller55(long value) {
        this.filler55 = value;
    }

    public boolean isSetFiller55() {
        return true;
    }

}

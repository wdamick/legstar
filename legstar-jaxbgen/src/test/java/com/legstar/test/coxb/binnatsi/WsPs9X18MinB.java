
package com.legstar.test.coxb.binnatsi;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for WsPs9X18MinB complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WsPs9X18MinB">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="Filler43">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}int">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler44">
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
@XmlType(name = "WsPs9X18MinB", propOrder = {
    "filler43",
    "filler44"
})
public class WsPs9X18MinB
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "Filler43")
    @CobolElement(cobolName = "FILLER", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 20, isSigned = true, totalDigits = 9, picture = "S9(9)", usage = "COMP-5", value = "-2147483648", srceLine = 43)
    protected int filler43 = -2147483648;
    @XmlElement(name = "Filler44")
    @CobolElement(cobolName = "FILLER", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 20, isSigned = false, totalDigits = 9, picture = "9(9)", usage = "COMP-5", value = "0", srceLine = 44)
    protected long filler44 = 0L;

    /**
     * Gets the value of the filler43 property.
     * 
     */
    public int getFiller43() {
        return filler43;
    }

    /**
     * Sets the value of the filler43 property.
     * 
     */
    public void setFiller43(int value) {
        this.filler43 = value;
    }

    public boolean isSetFiller43() {
        return true;
    }

    /**
     * Gets the value of the filler44 property.
     * 
     */
    public long getFiller44() {
        return filler44;
    }

    /**
     * Sets the value of the filler44 property.
     * 
     */
    public void setFiller44(long value) {
        this.filler44 = value;
    }

    public boolean isSetFiller44() {
        return true;
    }

}


package com.legstar.test.coxb.binnatus;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for WsP9X18MaxB complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WsP9X18MaxB">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="Filler50">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler51">
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
@XmlType(name = "WsP9X18MaxB", propOrder = {
    "filler50",
    "filler51"
})
public class WsP9X18MaxB
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "Filler50")
    @CobolElement(cobolName = "FILLER", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 20, isSigned = false, totalDigits = 9, picture = "9(9)", usage = "COMP-5", value = "4294967295", srceLine = 50)
    protected long filler50 = 4294967295L;
    @XmlElement(name = "Filler51")
    @CobolElement(cobolName = "FILLER", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 20, isSigned = false, totalDigits = 9, picture = "9(9)", usage = "COMP-5", value = "4294967295", srceLine = 51)
    protected long filler51 = 4294967295L;

    /**
     * Gets the value of the filler50 property.
     * 
     */
    public long getFiller50() {
        return filler50;
    }

    /**
     * Sets the value of the filler50 property.
     * 
     */
    public void setFiller50(long value) {
        this.filler50 = value;
    }

    public boolean isSetFiller50() {
        return true;
    }

    /**
     * Gets the value of the filler51 property.
     * 
     */
    public long getFiller51() {
        return filler51;
    }

    /**
     * Sets the value of the filler51 property.
     * 
     */
    public void setFiller51(long value) {
        this.filler51 = value;
    }

    public boolean isSetFiller51() {
        return true;
    }

}

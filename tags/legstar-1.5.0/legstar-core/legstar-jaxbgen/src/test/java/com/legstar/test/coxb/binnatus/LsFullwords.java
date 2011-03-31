
package com.legstar.test.coxb.binnatus;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for LsFullwords complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsFullwords">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsP9X9Min">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="LsP9X9Low">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="LsP9X9High">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="LsP9X9Max">
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
@XmlType(name = "LsFullwords", propOrder = {
    "lsP9X9Min",
    "lsP9X9Low",
    "lsP9X9High",
    "lsP9X9Max"
})
public class LsFullwords
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "LsP9X9Min")
    @CobolElement(cobolName = "LS-P9X9-MIN", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 15, isSigned = false, totalDigits = 9, picture = "9(9)", usage = "COMP-5", srceLine = 75)
    protected long lsP9X9Min;
    @XmlElement(name = "LsP9X9Low")
    @CobolElement(cobolName = "LS-P9X9-LOW", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 15, isSigned = false, totalDigits = 9, picture = "9(9)", usage = "COMP-5", srceLine = 76)
    protected long lsP9X9Low;
    @XmlElement(name = "LsP9X9High")
    @CobolElement(cobolName = "LS-P9X9-HIGH", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 15, isSigned = false, totalDigits = 9, picture = "9(9)", usage = "COMP-5", srceLine = 77)
    protected long lsP9X9High;
    @XmlElement(name = "LsP9X9Max")
    @CobolElement(cobolName = "LS-P9X9-MAX", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 15, isSigned = false, totalDigits = 9, picture = "9(9)", usage = "COMP-5", srceLine = 78)
    protected long lsP9X9Max;

    /**
     * Gets the value of the lsP9X9Min property.
     * 
     */
    public long getLsP9X9Min() {
        return lsP9X9Min;
    }

    /**
     * Sets the value of the lsP9X9Min property.
     * 
     */
    public void setLsP9X9Min(long value) {
        this.lsP9X9Min = value;
    }

    public boolean isSetLsP9X9Min() {
        return true;
    }

    /**
     * Gets the value of the lsP9X9Low property.
     * 
     */
    public long getLsP9X9Low() {
        return lsP9X9Low;
    }

    /**
     * Sets the value of the lsP9X9Low property.
     * 
     */
    public void setLsP9X9Low(long value) {
        this.lsP9X9Low = value;
    }

    public boolean isSetLsP9X9Low() {
        return true;
    }

    /**
     * Gets the value of the lsP9X9High property.
     * 
     */
    public long getLsP9X9High() {
        return lsP9X9High;
    }

    /**
     * Sets the value of the lsP9X9High property.
     * 
     */
    public void setLsP9X9High(long value) {
        this.lsP9X9High = value;
    }

    public boolean isSetLsP9X9High() {
        return true;
    }

    /**
     * Gets the value of the lsP9X9Max property.
     * 
     */
    public long getLsP9X9Max() {
        return lsP9X9Max;
    }

    /**
     * Sets the value of the lsP9X9Max property.
     * 
     */
    public void setLsP9X9Max(long value) {
        this.lsP9X9Max = value;
    }

    public boolean isSetLsP9X9Max() {
        return true;
    }

}

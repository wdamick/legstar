
package com.legstar.test.coxb.binarcht;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for LsSignedNative complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsSignedNative">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsPs9X4Min">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}short">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="LsPs9X4Max">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}short">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="LsPs9X9Min">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}int">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="LsPs9X9Max">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}int">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="LsPs9X18Min">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}long">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="LsPs9X18Max">
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
@XmlType(name = "LsSignedNative", propOrder = {
    "lsPs9X4Min",
    "lsPs9X4Max",
    "lsPs9X9Min",
    "lsPs9X9Max",
    "lsPs9X18Min",
    "lsPs9X18Max"
})
public class LsSignedNative
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "LsPs9X4Min")
    @CobolElement(cobolName = "LS-PS9X4-MIN", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 10, isSigned = true, totalDigits = 4, picture = "S9(4)", usage = "COMP-5", srceLine = 49)
    protected short lsPs9X4Min;
    @XmlElement(name = "LsPs9X4Max")
    @CobolElement(cobolName = "LS-PS9X4-MAX", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 10, isSigned = true, totalDigits = 4, picture = "S9(4)", usage = "COMP-5", srceLine = 50)
    protected short lsPs9X4Max;
    @XmlElement(name = "LsPs9X9Min")
    @CobolElement(cobolName = "LS-PS9X9-MIN", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 10, isSigned = true, totalDigits = 9, picture = "S9(9)", usage = "COMP-5", srceLine = 51)
    protected int lsPs9X9Min;
    @XmlElement(name = "LsPs9X9Max")
    @CobolElement(cobolName = "LS-PS9X9-MAX", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 10, isSigned = true, totalDigits = 9, picture = "S9(9)", usage = "COMP-5", srceLine = 52)
    protected int lsPs9X9Max;
    @XmlElement(name = "LsPs9X18Min")
    @CobolElement(cobolName = "LS-PS9X18-MIN", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 10, isSigned = true, totalDigits = 18, picture = "S9(18)", usage = "COMP-5", srceLine = 53)
    protected long lsPs9X18Min;
    @XmlElement(name = "LsPs9X18Max")
    @CobolElement(cobolName = "LS-PS9X18-MAX", type = CobolType.NATIVE_BINARY_ITEM, levelNumber = 10, isSigned = true, totalDigits = 18, picture = "S9(18)", usage = "COMP-5", srceLine = 54)
    protected long lsPs9X18Max;

    /**
     * Gets the value of the lsPs9X4Min property.
     * 
     */
    public short getLsPs9X4Min() {
        return lsPs9X4Min;
    }

    /**
     * Sets the value of the lsPs9X4Min property.
     * 
     */
    public void setLsPs9X4Min(short value) {
        this.lsPs9X4Min = value;
    }

    public boolean isSetLsPs9X4Min() {
        return true;
    }

    /**
     * Gets the value of the lsPs9X4Max property.
     * 
     */
    public short getLsPs9X4Max() {
        return lsPs9X4Max;
    }

    /**
     * Sets the value of the lsPs9X4Max property.
     * 
     */
    public void setLsPs9X4Max(short value) {
        this.lsPs9X4Max = value;
    }

    public boolean isSetLsPs9X4Max() {
        return true;
    }

    /**
     * Gets the value of the lsPs9X9Min property.
     * 
     */
    public int getLsPs9X9Min() {
        return lsPs9X9Min;
    }

    /**
     * Sets the value of the lsPs9X9Min property.
     * 
     */
    public void setLsPs9X9Min(int value) {
        this.lsPs9X9Min = value;
    }

    public boolean isSetLsPs9X9Min() {
        return true;
    }

    /**
     * Gets the value of the lsPs9X9Max property.
     * 
     */
    public int getLsPs9X9Max() {
        return lsPs9X9Max;
    }

    /**
     * Sets the value of the lsPs9X9Max property.
     * 
     */
    public void setLsPs9X9Max(int value) {
        this.lsPs9X9Max = value;
    }

    public boolean isSetLsPs9X9Max() {
        return true;
    }

    /**
     * Gets the value of the lsPs9X18Min property.
     * 
     */
    public long getLsPs9X18Min() {
        return lsPs9X18Min;
    }

    /**
     * Sets the value of the lsPs9X18Min property.
     * 
     */
    public void setLsPs9X18Min(long value) {
        this.lsPs9X18Min = value;
    }

    public boolean isSetLsPs9X18Min() {
        return true;
    }

    /**
     * Gets the value of the lsPs9X18Max property.
     * 
     */
    public long getLsPs9X18Max() {
        return lsPs9X18Max;
    }

    /**
     * Sets the value of the lsPs9X18Max property.
     * 
     */
    public void setLsPs9X18Max(long value) {
        this.lsPs9X18Max = value;
    }

    public boolean isSetLsPs9X18Max() {
        return true;
    }

}

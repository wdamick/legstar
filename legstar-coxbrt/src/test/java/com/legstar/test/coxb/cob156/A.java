package com.legstar.test.coxb.cob156;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;

/**
 * <p>
 * Java class for A complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="A">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="b">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *               &lt;totalDigits value="3"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="c" type="{http://com.legstar.test.coxb/cob156}C"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "A", propOrder = { "b", "c" })
public class A implements Serializable {

    private final static long serialVersionUID = 1L;
    @CobolElement(cobolName = "B", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 3, isODOObject = true, picture = "9(3)", srceLine = 2)
    protected int b;
    @XmlElement(required = true)
    @CobolElement(cobolName = "C", type = CobolType.GROUP_ITEM, levelNumber = 5, srceLine = 3)
    protected C c;

    /**
     * Gets the value of the b property.
     * 
     */
    public int getB() {
        return b;
    }

    /**
     * Sets the value of the b property.
     * 
     */
    public void setB(int value) {
        this.b = value;
    }

    public boolean isSetB() {
        return true;
    }

    /**
     * Gets the value of the c property.
     * 
     * @return possible object is {@link C }
     * 
     */
    public C getC() {
        return c;
    }

    /**
     * Sets the value of the c property.
     * 
     * @param value allowed object is {@link C }
     * 
     */
    public void setC(C value) {
        this.c = value;
    }

    public boolean isSetC() {
        return (this.c != null);
    }

}

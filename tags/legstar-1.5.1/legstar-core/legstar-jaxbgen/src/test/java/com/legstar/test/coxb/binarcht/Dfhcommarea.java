
package com.legstar.test.coxb.binarcht;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for Dfhcommarea complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Dfhcommarea">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsUnsignedNative" type="{http://legstar.com/test/coxb/binarcht}LsUnsignedNative"/>
 *         &lt;element name="LsSignedNative" type="{http://legstar.com/test/coxb/binarcht}LsSignedNative"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Dfhcommarea", propOrder = {
    "lsUnsignedNative",
    "lsSignedNative"
})
public class Dfhcommarea
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "LsUnsignedNative", required = true)
    @CobolElement(cobolName = "LS-UNSIGNED-NATIVE", type = CobolType.GROUP_ITEM, levelNumber = 5, srceLine = 41)
    protected LsUnsignedNative lsUnsignedNative;
    @XmlElement(name = "LsSignedNative", required = true)
    @CobolElement(cobolName = "LS-SIGNED-NATIVE", type = CobolType.GROUP_ITEM, levelNumber = 5, srceLine = 48)
    protected LsSignedNative lsSignedNative;

    /**
     * Gets the value of the lsUnsignedNative property.
     * 
     * @return
     *     possible object is
     *     {@link LsUnsignedNative }
     *     
     */
    public LsUnsignedNative getLsUnsignedNative() {
        return lsUnsignedNative;
    }

    /**
     * Sets the value of the lsUnsignedNative property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsUnsignedNative }
     *     
     */
    public void setLsUnsignedNative(LsUnsignedNative value) {
        this.lsUnsignedNative = value;
    }

    public boolean isSetLsUnsignedNative() {
        return (this.lsUnsignedNative!= null);
    }

    /**
     * Gets the value of the lsSignedNative property.
     * 
     * @return
     *     possible object is
     *     {@link LsSignedNative }
     *     
     */
    public LsSignedNative getLsSignedNative() {
        return lsSignedNative;
    }

    /**
     * Sets the value of the lsSignedNative property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsSignedNative }
     *     
     */
    public void setLsSignedNative(LsSignedNative value) {
        this.lsSignedNative = value;
    }

    public boolean isSetLsSignedNative() {
        return (this.lsSignedNative!= null);
    }

}

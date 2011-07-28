
package com.legstar.test.coxb.binpkdus;

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
 *         &lt;element name="LsUnsignedPackedDecimal" type="{http://legstar.com/test/coxb/binpkdus}LsUnsignedPackedDecimal"/>
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
    "lsUnsignedPackedDecimal"
})
public class Dfhcommarea
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "LsUnsignedPackedDecimal", required = true)
    @CobolElement(cobolName = "LS-UNSIGNED-PACKED-DECIMAL", type = CobolType.GROUP_ITEM, levelNumber = 5, srceLine = 56)
    protected LsUnsignedPackedDecimal lsUnsignedPackedDecimal;

    /**
     * Gets the value of the lsUnsignedPackedDecimal property.
     * 
     * @return
     *     possible object is
     *     {@link LsUnsignedPackedDecimal }
     *     
     */
    public LsUnsignedPackedDecimal getLsUnsignedPackedDecimal() {
        return lsUnsignedPackedDecimal;
    }

    /**
     * Sets the value of the lsUnsignedPackedDecimal property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsUnsignedPackedDecimal }
     *     
     */
    public void setLsUnsignedPackedDecimal(LsUnsignedPackedDecimal value) {
        this.lsUnsignedPackedDecimal = value;
    }

    public boolean isSetLsUnsignedPackedDecimal() {
        return (this.lsUnsignedPackedDecimal!= null);
    }

}

/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/

package com.legstar.test.coxb.binarcht;

import java.math.BigInteger;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsUnsignedNativeType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsUnsignedNativeType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsP9X4Min" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="LsP9X4Max" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="LsP9X9Min" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="LsP9X9Max" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="LsP9X18Min" type="{http://www.w3.org/2001/XMLSchema}integer"/>
 *         &lt;element name="LsP9X18Max" type="{http://www.w3.org/2001/XMLSchema}integer"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsUnsignedNativeType", propOrder = {
    "lsP9X4Min",
    "lsP9X4Max",
    "lsP9X9Min",
    "lsP9X9Max",
    "lsP9X18Min",
    "lsP9X18Max"
})
public class LsUnsignedNativeType {

    @XmlElement(name = "LsP9X4Min")
    protected int lsP9X4Min;
    @XmlElement(name = "LsP9X4Max")
    protected int lsP9X4Max;
    @XmlElement(name = "LsP9X9Min")
    protected long lsP9X9Min;
    @XmlElement(name = "LsP9X9Max")
    protected long lsP9X9Max;
    @XmlElement(name = "LsP9X18Min", required = true)
    protected BigInteger lsP9X18Min;
    @XmlElement(name = "LsP9X18Max", required = true)
    protected BigInteger lsP9X18Max;

    /**
     * Gets the value of the lsP9X4Min property.
     * 
     */
    public int getLsP9X4Min() {
        return lsP9X4Min;
    }

    /**
     * Sets the value of the lsP9X4Min property.
     * 
     */
    public void setLsP9X4Min(int value) {
        this.lsP9X4Min = value;
    }

    /**
     * Gets the value of the lsP9X4Max property.
     * 
     */
    public int getLsP9X4Max() {
        return lsP9X4Max;
    }

    /**
     * Sets the value of the lsP9X4Max property.
     * 
     */
    public void setLsP9X4Max(int value) {
        this.lsP9X4Max = value;
    }

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

    /**
     * Gets the value of the lsP9X18Min property.
     * 
     * @return
     *     possible object is
     *     {@link BigInteger }
     *     
     */
    public BigInteger getLsP9X18Min() {
        return lsP9X18Min;
    }

    /**
     * Sets the value of the lsP9X18Min property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigInteger }
     *     
     */
    public void setLsP9X18Min(BigInteger value) {
        this.lsP9X18Min = value;
    }

    /**
     * Gets the value of the lsP9X18Max property.
     * 
     * @return
     *     possible object is
     *     {@link BigInteger }
     *     
     */
    public BigInteger getLsP9X18Max() {
        return lsP9X18Max;
    }

    /**
     * Sets the value of the lsP9X18Max property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigInteger }
     *     
     */
    public void setLsP9X18Max(BigInteger value) {
        this.lsP9X18Max = value;
    }

}

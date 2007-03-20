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

package com.legstar.test.coxb.binpkdus;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsUnsignedPackedDecimalType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsUnsignedPackedDecimalType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsCompat" type="{http://legstar.com/test/coxb/binpkdus}LsCompatType"/>
 *         &lt;element name="LsExtend" type="{http://legstar.com/test/coxb/binpkdus}LsExtendType"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsUnsignedPackedDecimalType", propOrder = {
    "lsCompat",
    "lsExtend"
})
public class LsUnsignedPackedDecimalType {

    @XmlElement(name = "LsCompat", required = true)
    protected LsCompatType lsCompat;
    @XmlElement(name = "LsExtend", required = true)
    protected LsExtendType lsExtend;

    /**
     * Gets the value of the lsCompat property.
     * 
     * @return
     *     possible object is
     *     {@link LsCompatType }
     *     
     */
    public LsCompatType getLsCompat() {
        return lsCompat;
    }

    /**
     * Sets the value of the lsCompat property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsCompatType }
     *     
     */
    public void setLsCompat(LsCompatType value) {
        this.lsCompat = value;
    }

    /**
     * Gets the value of the lsExtend property.
     * 
     * @return
     *     possible object is
     *     {@link LsExtendType }
     *     
     */
    public LsExtendType getLsExtend() {
        return lsExtend;
    }

    /**
     * Sets the value of the lsExtend property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsExtendType }
     *     
     */
    public void setLsExtend(LsExtendType value) {
        this.lsExtend = value;
    }

}

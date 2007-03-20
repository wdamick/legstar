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
//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.1-b02-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.01.26 at 06:16:45 PM CET 
//


package com.legstar.test.coxb.binpkdus;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolType;
import com.legstar.coxb.annotation.CobolElement;


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
    @CobolElement(cobolName = "LS-COMPAT", type = CobolType.GROUP_ITEM, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false)
    protected LsCompatType lsCompat;
    @XmlElement(name = "LsExtend", required = true)
    @CobolElement(cobolName = "LS-EXTEND", type = CobolType.GROUP_ITEM, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false)
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

    public boolean isSetLsCompat() {
        return (this.lsCompat!= null);
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

    public boolean isSetLsExtend() {
        return (this.lsExtend!= null);
    }

}

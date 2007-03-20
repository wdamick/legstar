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
// Generated on: 2007.01.26 at 06:16:43 PM CET 
//


package com.legstar.test.coxb.binnatus;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolType;
import com.legstar.coxb.annotation.CobolElement;


/**
 * <p>Java class for LsHalfwordsType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsHalfwordsType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsP9X4Min">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="LsP9X4Low">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="LsP9X4High">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="LsP9X4Max">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
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
@XmlType(name = "LsHalfwordsType", propOrder = {
    "lsP9X4Min",
    "lsP9X4Low",
    "lsP9X4High",
    "lsP9X4Max"
})
public class LsHalfwordsType {

    @XmlElement(name = "LsP9X4Min")
    @CobolElement(cobolName = "LS-P9X4-MIN", type = CobolType.NATIVE_BINARY_ITEM, byteLength = 2, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false, totalDigits = 4)
    protected int lsP9X4Min;
    @XmlElement(name = "LsP9X4Low")
    @CobolElement(cobolName = "LS-P9X4-LOW", type = CobolType.NATIVE_BINARY_ITEM, byteLength = 2, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false, totalDigits = 4)
    protected int lsP9X4Low;
    @XmlElement(name = "LsP9X4High")
    @CobolElement(cobolName = "LS-P9X4-HIGH", type = CobolType.NATIVE_BINARY_ITEM, byteLength = 2, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false, totalDigits = 4)
    protected int lsP9X4High;
    @XmlElement(name = "LsP9X4Max")
    @CobolElement(cobolName = "LS-P9X4-MAX", type = CobolType.NATIVE_BINARY_ITEM, byteLength = 2, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false, totalDigits = 4)
    protected int lsP9X4Max;

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

    public boolean isSetLsP9X4Min() {
        return true;
    }

    /**
     * Gets the value of the lsP9X4Low property.
     * 
     */
    public int getLsP9X4Low() {
        return lsP9X4Low;
    }

    /**
     * Sets the value of the lsP9X4Low property.
     * 
     */
    public void setLsP9X4Low(int value) {
        this.lsP9X4Low = value;
    }

    public boolean isSetLsP9X4Low() {
        return true;
    }

    /**
     * Gets the value of the lsP9X4High property.
     * 
     */
    public int getLsP9X4High() {
        return lsP9X4High;
    }

    /**
     * Sets the value of the lsP9X4High property.
     * 
     */
    public void setLsP9X4High(int value) {
        this.lsP9X4High = value;
    }

    public boolean isSetLsP9X4High() {
        return true;
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

    public boolean isSetLsP9X4Max() {
        return true;
    }

}

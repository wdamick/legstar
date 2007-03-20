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
// Generated on: 2007.01.26 at 06:17:03 PM CET 
//


package com.legstar.test.coxb.redbotha;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolType;
import com.legstar.coxb.annotation.CobolElement;


/**
 * <p>Java class for Filler22Type complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Filler22Type">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CLeftByte">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;length value="1"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="CRightByte">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;length value="1"/>
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
@XmlType(name = "Filler22Type", propOrder = {
    "cLeftByte",
    "cRightByte"
})
public class Filler22Type {

    @XmlElement(name = "CLeftByte", required = true)
    @CobolElement(cobolName = "C-LEFT-BYTE", type = CobolType.ALPHANUMERIC_ITEM, byteLength = 1, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false)
    protected String cLeftByte;
    @XmlElement(name = "CRightByte", required = true)
    @CobolElement(cobolName = "C-RIGHT-BYTE", type = CobolType.ALPHANUMERIC_ITEM, byteLength = 1, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false)
    protected String cRightByte;

    /**
     * Gets the value of the cLeftByte property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCLeftByte() {
        return cLeftByte;
    }

    /**
     * Sets the value of the cLeftByte property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCLeftByte(String value) {
        this.cLeftByte = value;
    }

    public boolean isSetCLeftByte() {
        return (this.cLeftByte!= null);
    }

    /**
     * Gets the value of the cRightByte property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCRightByte() {
        return cRightByte;
    }

    /**
     * Sets the value of the cRightByte property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCRightByte(String value) {
        this.cRightByte = value;
    }

    public boolean isSetCRightByte() {
        return (this.cRightByte!= null);
    }

}

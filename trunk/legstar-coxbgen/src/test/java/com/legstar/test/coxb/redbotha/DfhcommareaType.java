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
 * <p>Java class for DfhcommareaType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="DfhcommareaType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;choice>
 *           &lt;element name="CNumeric">
 *             &lt;simpleType>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *                 &lt;minInclusive value="0"/>
 *                 &lt;maxInclusive value="9999"/>
 *               &lt;/restriction>
 *             &lt;/simpleType>
 *           &lt;/element>
 *           &lt;element name="Filler22" type="{http://legstar.com/test/coxb/redbotha}Filler22Type"/>
 *         &lt;/choice>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "DfhcommareaType", propOrder = {
    "cNumeric",
    "filler22"
})
public class DfhcommareaType {

    @XmlElement(name = "CNumeric")
    @CobolElement(cobolName = "C-NUMERIC", type = CobolType.BINARY_ITEM, byteLength = 2, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false, totalDigits = 4, isRedefined = true, unmarshalChoiceStrategyClassName = "com.legstar.coxb.cust.redbotha.ChoiceSelector")
    protected Integer cNumeric;
    @XmlElement(name = "Filler22")
    @CobolElement(cobolName = "FILLER-22", type = CobolType.GROUP_ITEM, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false, redefines = "C-NUMERIC")
    protected Filler22Type filler22;

    /**
     * Gets the value of the cNumeric property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getCNumeric() {
        return cNumeric;
    }

    /**
     * Sets the value of the cNumeric property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setCNumeric(Integer value) {
        this.cNumeric = value;
    }

    public boolean isSetCNumeric() {
        return (this.cNumeric!= null);
    }

    /**
     * Gets the value of the filler22 property.
     * 
     * @return
     *     possible object is
     *     {@link Filler22Type }
     *     
     */
    public Filler22Type getFiller22() {
        return filler22;
    }

    /**
     * Sets the value of the filler22 property.
     * 
     * @param value
     *     allowed object is
     *     {@link Filler22Type }
     *     
     */
    public void setFiller22(Filler22Type value) {
        this.filler22 = value;
    }

    public boolean isSetFiller22() {
        return (this.filler22 != null);
    }

}

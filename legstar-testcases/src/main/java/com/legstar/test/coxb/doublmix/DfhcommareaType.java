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
// Generated on: 2007.01.26 at 06:16:47 PM CET 
//


package com.legstar.test.coxb.doublmix;

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
 *         &lt;element name="CDouble1234">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}double">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="CDouble0">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}double">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="CDouble1">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}double">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="CDouble345006P5678">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}double">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="CDouble798P20067Em16">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}double">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="CDouble3P40282347Ep38">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}double">
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
@XmlType(name = "DfhcommareaType", propOrder = {
    "cDouble1234",
    "cDouble0",
    "cDouble1",
    "cDouble345006P5678",
    "cDouble798P20067Em16",
    "cDouble3P40282347Ep38"
})
public class DfhcommareaType {

    @XmlElement(name = "CDouble1234")
    @CobolElement(cobolName = "C-DOUBLE-1234", type = CobolType.DOUBLE_FLOAT_ITEM, byteLength = 8, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false)
    protected double cDouble1234;
    @XmlElement(name = "CDouble0")
    @CobolElement(cobolName = "C-DOUBLE-0", type = CobolType.DOUBLE_FLOAT_ITEM, byteLength = 8, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false)
    protected double cDouble0;
    @XmlElement(name = "CDouble1")
    @CobolElement(cobolName = "C-DOUBLE-1", type = CobolType.DOUBLE_FLOAT_ITEM, byteLength = 8, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false)
    protected double cDouble1;
    @XmlElement(name = "CDouble345006P5678")
    @CobolElement(cobolName = "C-DOUBLE-345006p5678", type = CobolType.DOUBLE_FLOAT_ITEM, byteLength = 8, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false)
    protected double cDouble345006P5678;
    @XmlElement(name = "CDouble798P20067Em16")
    @CobolElement(cobolName = "C-DOUBLE-798p20067em16", type = CobolType.DOUBLE_FLOAT_ITEM, byteLength = 8, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false)
    protected double cDouble798P20067Em16;
    @XmlElement(name = "CDouble3P40282347Ep38")
    @CobolElement(cobolName = "C-DOUBLE-3p40282347ep38", type = CobolType.DOUBLE_FLOAT_ITEM, byteLength = 8, isJustifiedRight = false, isSigned = false, isSignLeading = false, isSignSeparate = false)
    protected double cDouble3P40282347Ep38;

    /**
     * Gets the value of the cDouble1234 property.
     * 
     */
    public double getCDouble1234() {
        return cDouble1234;
    }

    /**
     * Sets the value of the cDouble1234 property.
     * 
     */
    public void setCDouble1234(double value) {
        this.cDouble1234 = value;
    }

    public boolean isSetCDouble1234() {
        return true;
    }

    /**
     * Gets the value of the cDouble0 property.
     * 
     */
    public double getCDouble0() {
        return cDouble0;
    }

    /**
     * Sets the value of the cDouble0 property.
     * 
     */
    public void setCDouble0(double value) {
        this.cDouble0 = value;
    }

    public boolean isSetCDouble0() {
        return true;
    }

    /**
     * Gets the value of the cDouble1 property.
     * 
     */
    public double getCDouble1() {
        return cDouble1;
    }

    /**
     * Sets the value of the cDouble1 property.
     * 
     */
    public void setCDouble1(double value) {
        this.cDouble1 = value;
    }

    public boolean isSetCDouble1() {
        return true;
    }

    /**
     * Gets the value of the cDouble345006P5678 property.
     * 
     */
    public double getCDouble345006P5678() {
        return cDouble345006P5678;
    }

    /**
     * Sets the value of the cDouble345006P5678 property.
     * 
     */
    public void setCDouble345006P5678(double value) {
        this.cDouble345006P5678 = value;
    }

    public boolean isSetCDouble345006P5678() {
        return true;
    }

    /**
     * Gets the value of the cDouble798P20067Em16 property.
     * 
     */
    public double getCDouble798P20067Em16() {
        return cDouble798P20067Em16;
    }

    /**
     * Sets the value of the cDouble798P20067Em16 property.
     * 
     */
    public void setCDouble798P20067Em16(double value) {
        this.cDouble798P20067Em16 = value;
    }

    public boolean isSetCDouble798P20067Em16() {
        return true;
    }

    /**
     * Gets the value of the cDouble3P40282347Ep38 property.
     * 
     */
    public double getCDouble3P40282347Ep38() {
        return cDouble3P40282347Ep38;
    }

    /**
     * Sets the value of the cDouble3P40282347Ep38 property.
     * 
     */
    public void setCDouble3P40282347Ep38(double value) {
        this.cDouble3P40282347Ep38 = value;
    }

    public boolean isSetCDouble3P40282347Ep38() {
        return true;
    }

}

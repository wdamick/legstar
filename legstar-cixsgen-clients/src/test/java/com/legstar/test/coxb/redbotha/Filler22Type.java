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

package com.legstar.test.coxb.redbotha;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


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
 *         &lt;element name="CLeftByte" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="CRightByte" type="{http://www.w3.org/2001/XMLSchema}string"/>
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
    protected String cLeftByte;
    @XmlElement(name = "CRightByte", required = true)
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

}

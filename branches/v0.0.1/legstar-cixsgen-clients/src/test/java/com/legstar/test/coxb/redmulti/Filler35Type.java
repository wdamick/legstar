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

package com.legstar.test.coxb.redmulti;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for Filler35Type complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Filler35Type">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CString" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="Filler37" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Filler35Type", propOrder = {
    "cString",
    "filler37"
})
public class Filler35Type {

    @XmlElement(name = "CString", required = true)
    protected String cString;
    @XmlElement(name = "Filler37", required = true)
    protected String filler37;

    /**
     * Gets the value of the cString property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCString() {
        return cString;
    }

    /**
     * Sets the value of the cString property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCString(String value) {
        this.cString = value;
    }

    /**
     * Gets the value of the filler37 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller37() {
        return filler37;
    }

    /**
     * Sets the value of the filler37 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller37(String value) {
        this.filler37 = value;
    }

}

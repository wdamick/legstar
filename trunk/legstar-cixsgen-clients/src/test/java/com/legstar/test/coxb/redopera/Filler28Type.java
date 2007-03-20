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

package com.legstar.test.coxb.redopera;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for Filler28Type complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Filler28Type">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CInteger" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="Filler30" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Filler28Type", propOrder = {
    "cInteger",
    "filler30"
})
public class Filler28Type {

    @XmlElement(name = "CInteger")
    protected int cInteger;
    @XmlElement(name = "Filler30", required = true)
    protected String filler30;

    /**
     * Gets the value of the cInteger property.
     * 
     */
    public int getCInteger() {
        return cInteger;
    }

    /**
     * Sets the value of the cInteger property.
     * 
     */
    public void setCInteger(int value) {
        this.cInteger = value;
    }

    /**
     * Gets the value of the filler30 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller30() {
        return filler30;
    }

    /**
     * Sets the value of the filler30 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller30(String value) {
        this.filler30 = value;
    }

}

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

package com.legstar.test.coxb.fixarcom;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for CArrayType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="CArrayType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CItem1" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="CItem2" type="{http://www.w3.org/2001/XMLSchema}short"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "CArrayType", propOrder = {
    "cItem1",
    "cItem2"
})
public class CArrayType {

    @XmlElement(name = "CItem1", required = true)
    protected String cItem1;
    @XmlElement(name = "CItem2")
    protected short cItem2;

    /**
     * Gets the value of the cItem1 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCItem1() {
        return cItem1;
    }

    /**
     * Sets the value of the cItem1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCItem1(String value) {
        this.cItem1 = value;
    }

    /**
     * Gets the value of the cItem2 property.
     * 
     */
    public short getCItem2() {
        return cItem2;
    }

    /**
     * Sets the value of the cItem2 property.
     * 
     */
    public void setCItem2(short value) {
        this.cItem2 = value;
    }

}

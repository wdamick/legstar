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
 * <p>Java class for Filler38Type complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Filler38Type">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CErrorNum" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="CErrorDescription" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Filler38Type", propOrder = {
    "cErrorNum",
    "cErrorDescription"
})
public class Filler38Type {

    @XmlElement(name = "CErrorNum")
    protected int cErrorNum;
    @XmlElement(name = "CErrorDescription", required = true)
    protected String cErrorDescription;

    /**
     * Gets the value of the cErrorNum property.
     * 
     */
    public int getCErrorNum() {
        return cErrorNum;
    }

    /**
     * Sets the value of the cErrorNum property.
     * 
     */
    public void setCErrorNum(int value) {
        this.cErrorNum = value;
    }

    /**
     * Gets the value of the cErrorDescription property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCErrorDescription() {
        return cErrorDescription;
    }

    /**
     * Sets the value of the cErrorDescription property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCErrorDescription(String value) {
        this.cErrorDescription = value;
    }

}

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

package com.legstar.test.coxb.lsfileae;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ComPersonalType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ComPersonalType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ComName" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="ComAddress" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="ComPhone" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ComPersonalType", propOrder = {
    "comName",
    "comAddress",
    "comPhone"
})
public class ComPersonalType {

    @XmlElement(name = "ComName", required = true)
    protected String comName;
    @XmlElement(name = "ComAddress", required = true)
    protected String comAddress;
    @XmlElement(name = "ComPhone", required = true)
    protected String comPhone;

    /**
     * Gets the value of the comName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getComName() {
        return comName;
    }

    /**
     * Sets the value of the comName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setComName(String value) {
        this.comName = value;
    }

    /**
     * Gets the value of the comAddress property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getComAddress() {
        return comAddress;
    }

    /**
     * Sets the value of the comAddress property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setComAddress(String value) {
        this.comAddress = value;
    }

    /**
     * Gets the value of the comPhone property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getComPhone() {
        return comPhone;
    }

    /**
     * Sets the value of the comPhone property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setComPhone(String value) {
        this.comPhone = value;
    }

}

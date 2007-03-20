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

package com.legstar.test.coxb.dplarcht;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsSearchCriteriaType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsSearchCriteriaType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsStartwith" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="LsStartwithLen" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsSearchCriteriaType", propOrder = {
    "lsStartwith",
    "lsStartwithLen"
})
public class LsSearchCriteriaType {

    @XmlElement(name = "LsStartwith", required = true)
    protected String lsStartwith;
    @XmlElement(name = "LsStartwithLen")
    protected long lsStartwithLen;

    /**
     * Gets the value of the lsStartwith property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLsStartwith() {
        return lsStartwith;
    }

    /**
     * Sets the value of the lsStartwith property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLsStartwith(String value) {
        this.lsStartwith = value;
    }

    /**
     * Gets the value of the lsStartwithLen property.
     * 
     */
    public long getLsStartwithLen() {
        return lsStartwithLen;
    }

    /**
     * Sets the value of the lsStartwithLen property.
     * 
     */
    public void setLsStartwithLen(long value) {
        this.lsStartwithLen = value;
    }

}

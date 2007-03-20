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

package com.legstar.test.coxb.binnatus;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsUnsignedNativeType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsUnsignedNativeType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsHalfwords" type="{http://legstar.com/test/coxb/binnatus}LsHalfwordsType"/>
 *         &lt;element name="LsFullwords" type="{http://legstar.com/test/coxb/binnatus}LsFullwordsType"/>
 *         &lt;element name="LsDoublewords" type="{http://legstar.com/test/coxb/binnatus}LsDoublewordsType"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsUnsignedNativeType", propOrder = {
    "lsHalfwords",
    "lsFullwords",
    "lsDoublewords"
})
public class LsUnsignedNativeType {

    @XmlElement(name = "LsHalfwords", required = true)
    protected LsHalfwordsType lsHalfwords;
    @XmlElement(name = "LsFullwords", required = true)
    protected LsFullwordsType lsFullwords;
    @XmlElement(name = "LsDoublewords", required = true)
    protected LsDoublewordsType lsDoublewords;

    /**
     * Gets the value of the lsHalfwords property.
     * 
     * @return
     *     possible object is
     *     {@link LsHalfwordsType }
     *     
     */
    public LsHalfwordsType getLsHalfwords() {
        return lsHalfwords;
    }

    /**
     * Sets the value of the lsHalfwords property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsHalfwordsType }
     *     
     */
    public void setLsHalfwords(LsHalfwordsType value) {
        this.lsHalfwords = value;
    }

    /**
     * Gets the value of the lsFullwords property.
     * 
     * @return
     *     possible object is
     *     {@link LsFullwordsType }
     *     
     */
    public LsFullwordsType getLsFullwords() {
        return lsFullwords;
    }

    /**
     * Sets the value of the lsFullwords property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsFullwordsType }
     *     
     */
    public void setLsFullwords(LsFullwordsType value) {
        this.lsFullwords = value;
    }

    /**
     * Gets the value of the lsDoublewords property.
     * 
     * @return
     *     possible object is
     *     {@link LsDoublewordsType }
     *     
     */
    public LsDoublewordsType getLsDoublewords() {
        return lsDoublewords;
    }

    /**
     * Sets the value of the lsDoublewords property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsDoublewordsType }
     *     
     */
    public void setLsDoublewords(LsDoublewordsType value) {
        this.lsDoublewords = value;
    }

}

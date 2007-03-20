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
 * <p>Java class for LsFullwordsType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsFullwordsType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsP9X9Min" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="LsP9X9Low" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="LsP9X9High" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="LsP9X9Max" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsFullwordsType", propOrder = {
    "lsP9X9Min",
    "lsP9X9Low",
    "lsP9X9High",
    "lsP9X9Max"
})
public class LsFullwordsType {

    @XmlElement(name = "LsP9X9Min")
    protected long lsP9X9Min;
    @XmlElement(name = "LsP9X9Low")
    protected long lsP9X9Low;
    @XmlElement(name = "LsP9X9High")
    protected long lsP9X9High;
    @XmlElement(name = "LsP9X9Max")
    protected long lsP9X9Max;

    /**
     * Gets the value of the lsP9X9Min property.
     * 
     */
    public long getLsP9X9Min() {
        return lsP9X9Min;
    }

    /**
     * Sets the value of the lsP9X9Min property.
     * 
     */
    public void setLsP9X9Min(long value) {
        this.lsP9X9Min = value;
    }

    /**
     * Gets the value of the lsP9X9Low property.
     * 
     */
    public long getLsP9X9Low() {
        return lsP9X9Low;
    }

    /**
     * Sets the value of the lsP9X9Low property.
     * 
     */
    public void setLsP9X9Low(long value) {
        this.lsP9X9Low = value;
    }

    /**
     * Gets the value of the lsP9X9High property.
     * 
     */
    public long getLsP9X9High() {
        return lsP9X9High;
    }

    /**
     * Sets the value of the lsP9X9High property.
     * 
     */
    public void setLsP9X9High(long value) {
        this.lsP9X9High = value;
    }

    /**
     * Gets the value of the lsP9X9Max property.
     * 
     */
    public long getLsP9X9Max() {
        return lsP9X9Max;
    }

    /**
     * Sets the value of the lsP9X9Max property.
     * 
     */
    public void setLsP9X9Max(long value) {
        this.lsP9X9Max = value;
    }

}

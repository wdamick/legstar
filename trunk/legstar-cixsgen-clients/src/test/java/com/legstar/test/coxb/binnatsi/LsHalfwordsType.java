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

package com.legstar.test.coxb.binnatsi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsHalfwordsType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsHalfwordsType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsPs9X4Min" type="{http://www.w3.org/2001/XMLSchema}short"/>
 *         &lt;element name="LsPs9X4Low" type="{http://www.w3.org/2001/XMLSchema}short"/>
 *         &lt;element name="LsPs9X4High" type="{http://www.w3.org/2001/XMLSchema}short"/>
 *         &lt;element name="LsPs9X4Max" type="{http://www.w3.org/2001/XMLSchema}short"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsHalfwordsType", propOrder = {
    "lsPs9X4Min",
    "lsPs9X4Low",
    "lsPs9X4High",
    "lsPs9X4Max"
})
public class LsHalfwordsType {

    @XmlElement(name = "LsPs9X4Min")
    protected short lsPs9X4Min;
    @XmlElement(name = "LsPs9X4Low")
    protected short lsPs9X4Low;
    @XmlElement(name = "LsPs9X4High")
    protected short lsPs9X4High;
    @XmlElement(name = "LsPs9X4Max")
    protected short lsPs9X4Max;

    /**
     * Gets the value of the lsPs9X4Min property.
     * 
     */
    public short getLsPs9X4Min() {
        return lsPs9X4Min;
    }

    /**
     * Sets the value of the lsPs9X4Min property.
     * 
     */
    public void setLsPs9X4Min(short value) {
        this.lsPs9X4Min = value;
    }

    /**
     * Gets the value of the lsPs9X4Low property.
     * 
     */
    public short getLsPs9X4Low() {
        return lsPs9X4Low;
    }

    /**
     * Sets the value of the lsPs9X4Low property.
     * 
     */
    public void setLsPs9X4Low(short value) {
        this.lsPs9X4Low = value;
    }

    /**
     * Gets the value of the lsPs9X4High property.
     * 
     */
    public short getLsPs9X4High() {
        return lsPs9X4High;
    }

    /**
     * Sets the value of the lsPs9X4High property.
     * 
     */
    public void setLsPs9X4High(short value) {
        this.lsPs9X4High = value;
    }

    /**
     * Gets the value of the lsPs9X4Max property.
     * 
     */
    public short getLsPs9X4Max() {
        return lsPs9X4Max;
    }

    /**
     * Sets the value of the lsPs9X4Max property.
     * 
     */
    public void setLsPs9X4Max(short value) {
        this.lsPs9X4Max = value;
    }

}

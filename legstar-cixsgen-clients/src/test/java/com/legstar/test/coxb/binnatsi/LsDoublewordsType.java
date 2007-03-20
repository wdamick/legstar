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
 * <p>Java class for LsDoublewordsType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsDoublewordsType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsPs9X18Min" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="LsPs9X18Low" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="LsPs9X18High" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="LsPs9X18Max" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsDoublewordsType", propOrder = {
    "lsPs9X18Min",
    "lsPs9X18Low",
    "lsPs9X18High",
    "lsPs9X18Max"
})
public class LsDoublewordsType {

    @XmlElement(name = "LsPs9X18Min")
    protected long lsPs9X18Min;
    @XmlElement(name = "LsPs9X18Low")
    protected long lsPs9X18Low;
    @XmlElement(name = "LsPs9X18High")
    protected long lsPs9X18High;
    @XmlElement(name = "LsPs9X18Max")
    protected long lsPs9X18Max;

    /**
     * Gets the value of the lsPs9X18Min property.
     * 
     */
    public long getLsPs9X18Min() {
        return lsPs9X18Min;
    }

    /**
     * Sets the value of the lsPs9X18Min property.
     * 
     */
    public void setLsPs9X18Min(long value) {
        this.lsPs9X18Min = value;
    }

    /**
     * Gets the value of the lsPs9X18Low property.
     * 
     */
    public long getLsPs9X18Low() {
        return lsPs9X18Low;
    }

    /**
     * Sets the value of the lsPs9X18Low property.
     * 
     */
    public void setLsPs9X18Low(long value) {
        this.lsPs9X18Low = value;
    }

    /**
     * Gets the value of the lsPs9X18High property.
     * 
     */
    public long getLsPs9X18High() {
        return lsPs9X18High;
    }

    /**
     * Sets the value of the lsPs9X18High property.
     * 
     */
    public void setLsPs9X18High(long value) {
        this.lsPs9X18High = value;
    }

    /**
     * Gets the value of the lsPs9X18Max property.
     * 
     */
    public long getLsPs9X18Max() {
        return lsPs9X18Max;
    }

    /**
     * Sets the value of the lsPs9X18Max property.
     * 
     */
    public void setLsPs9X18Max(long value) {
        this.lsPs9X18Max = value;
    }

}

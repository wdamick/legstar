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
 * <p>Java class for LsReplyType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsReplyType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsReplyType" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="LsReplyData" type="{http://legstar.com/test/coxb/dplarcht}LsReplyDataType"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsReplyType", propOrder = {
    "lsReplyType",
    "lsReplyData"
})
public class LsReplyType {

    @XmlElement(name = "LsReplyType")
    protected int lsReplyType;
    @XmlElement(name = "LsReplyData", required = true)
    protected LsReplyDataType lsReplyData;

    /**
     * Gets the value of the lsReplyType property.
     * 
     */
    public int getLsReplyType() {
        return lsReplyType;
    }

    /**
     * Sets the value of the lsReplyType property.
     * 
     */
    public void setLsReplyType(int value) {
        this.lsReplyType = value;
    }

    /**
     * Gets the value of the lsReplyData property.
     * 
     * @return
     *     possible object is
     *     {@link LsReplyDataType }
     *     
     */
    public LsReplyDataType getLsReplyData() {
        return lsReplyData;
    }

    /**
     * Sets the value of the lsReplyData property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsReplyDataType }
     *     
     */
    public void setLsReplyData(LsReplyDataType value) {
        this.lsReplyData = value;
    }

}

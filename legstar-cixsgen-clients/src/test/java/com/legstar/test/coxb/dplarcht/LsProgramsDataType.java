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
 * <p>Java class for LsProgramsDataType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsProgramsDataType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsProgramName" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="LsProgramType" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="LsProgramLanguage" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="LsProgramLength" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="LsProgramUsecount" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="Filler113" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsProgramsDataType", propOrder = {
    "lsProgramName",
    "lsProgramType",
    "lsProgramLanguage",
    "lsProgramLength",
    "lsProgramUsecount",
    "filler113"
})
public class LsProgramsDataType {

    @XmlElement(name = "LsProgramName", required = true)
    protected String lsProgramName;
    @XmlElement(name = "LsProgramType", required = true)
    protected String lsProgramType;
    @XmlElement(name = "LsProgramLanguage", required = true)
    protected String lsProgramLanguage;
    @XmlElement(name = "LsProgramLength")
    protected int lsProgramLength;
    @XmlElement(name = "LsProgramUsecount")
    protected int lsProgramUsecount;
    @XmlElement(name = "Filler113", required = true)
    protected String filler113;

    /**
     * Gets the value of the lsProgramName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLsProgramName() {
        return lsProgramName;
    }

    /**
     * Sets the value of the lsProgramName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLsProgramName(String value) {
        this.lsProgramName = value;
    }

    /**
     * Gets the value of the lsProgramType property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLsProgramType() {
        return lsProgramType;
    }

    /**
     * Sets the value of the lsProgramType property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLsProgramType(String value) {
        this.lsProgramType = value;
    }

    /**
     * Gets the value of the lsProgramLanguage property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLsProgramLanguage() {
        return lsProgramLanguage;
    }

    /**
     * Sets the value of the lsProgramLanguage property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLsProgramLanguage(String value) {
        this.lsProgramLanguage = value;
    }

    /**
     * Gets the value of the lsProgramLength property.
     * 
     */
    public int getLsProgramLength() {
        return lsProgramLength;
    }

    /**
     * Sets the value of the lsProgramLength property.
     * 
     */
    public void setLsProgramLength(int value) {
        this.lsProgramLength = value;
    }

    /**
     * Gets the value of the lsProgramUsecount property.
     * 
     */
    public int getLsProgramUsecount() {
        return lsProgramUsecount;
    }

    /**
     * Sets the value of the lsProgramUsecount property.
     * 
     */
    public void setLsProgramUsecount(int value) {
        this.lsProgramUsecount = value;
    }

    /**
     * Gets the value of the filler113 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller113() {
        return filler113;
    }

    /**
     * Sets the value of the filler113 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller113(String value) {
        this.filler113 = value;
    }

}

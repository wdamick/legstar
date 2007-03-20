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

package com.legstar.test.cixs.fixarsim;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for FixarsimHostHeader complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="FixarsimHostHeader">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="hostUser" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="hostPassword" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="hostIPAddress" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="hostIPPort" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="hostCICWPath" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "FixarsimHostHeader", propOrder = {
    "hostUser",
    "hostPassword",
    "hostIPAddress",
    "hostIPPort",
    "hostCICWPath"
})
public class FixarsimHostHeader {

    @XmlElement(required = true)
    protected String hostUser;
    @XmlElement(required = true)
    protected String hostPassword;
    @XmlElement(required = true)
    protected String hostIPAddress;
    protected int hostIPPort;
    @XmlElement(required = true)
    protected String hostCICWPath;

    /**
     * Gets the value of the hostUser property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getHostUser() {
        return hostUser;
    }

    /**
     * Sets the value of the hostUser property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setHostUser(String value) {
        this.hostUser = value;
    }

    /**
     * Gets the value of the hostPassword property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getHostPassword() {
        return hostPassword;
    }

    /**
     * Sets the value of the hostPassword property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setHostPassword(String value) {
        this.hostPassword = value;
    }

    /**
     * Gets the value of the hostIPAddress property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getHostIPAddress() {
        return hostIPAddress;
    }

    /**
     * Sets the value of the hostIPAddress property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setHostIPAddress(String value) {
        this.hostIPAddress = value;
    }

    /**
     * Gets the value of the hostIPPort property.
     * 
     */
    public int getHostIPPort() {
        return hostIPPort;
    }

    /**
     * Sets the value of the hostIPPort property.
     * 
     */
    public void setHostIPPort(int value) {
        this.hostIPPort = value;
    }

    /**
     * Gets the value of the hostCICWPath property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getHostCICWPath() {
        return hostCICWPath;
    }

    /**
     * Sets the value of the hostCICWPath property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setHostCICWPath(String value) {
        this.hostCICWPath = value;
    }

}

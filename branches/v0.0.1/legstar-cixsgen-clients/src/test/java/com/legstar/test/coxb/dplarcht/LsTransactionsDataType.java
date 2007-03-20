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
 * <p>Java class for LsTransactionsDataType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsTransactionsDataType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsTransactionName" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="LsTransactionProgram" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="LsTransactionStatus" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="Filler119" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsTransactionsDataType", propOrder = {
    "lsTransactionName",
    "lsTransactionProgram",
    "lsTransactionStatus",
    "filler119"
})
public class LsTransactionsDataType {

    @XmlElement(name = "LsTransactionName", required = true)
    protected String lsTransactionName;
    @XmlElement(name = "LsTransactionProgram", required = true)
    protected String lsTransactionProgram;
    @XmlElement(name = "LsTransactionStatus", required = true)
    protected String lsTransactionStatus;
    @XmlElement(name = "Filler119", required = true)
    protected String filler119;

    /**
     * Gets the value of the lsTransactionName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLsTransactionName() {
        return lsTransactionName;
    }

    /**
     * Sets the value of the lsTransactionName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLsTransactionName(String value) {
        this.lsTransactionName = value;
    }

    /**
     * Gets the value of the lsTransactionProgram property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLsTransactionProgram() {
        return lsTransactionProgram;
    }

    /**
     * Sets the value of the lsTransactionProgram property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLsTransactionProgram(String value) {
        this.lsTransactionProgram = value;
    }

    /**
     * Gets the value of the lsTransactionStatus property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLsTransactionStatus() {
        return lsTransactionStatus;
    }

    /**
     * Sets the value of the lsTransactionStatus property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLsTransactionStatus(String value) {
        this.lsTransactionStatus = value;
    }

    /**
     * Gets the value of the filler119 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller119() {
        return filler119;
    }

    /**
     * Sets the value of the filler119 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller119(String value) {
        this.filler119 = value;
    }

}

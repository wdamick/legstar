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

package com.legstar.test.coxb.lsfileal;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ReplyErrorHeaderType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ReplyErrorHeaderType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ReplyResp" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="ReplyResp2" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="ReplyMessage" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ReplyErrorHeaderType", propOrder = {
    "replyResp",
    "replyResp2",
    "replyMessage"
})
public class ReplyErrorHeaderType {

    @XmlElement(name = "ReplyResp")
    protected int replyResp;
    @XmlElement(name = "ReplyResp2")
    protected int replyResp2;
    @XmlElement(name = "ReplyMessage", required = true)
    protected String replyMessage;

    /**
     * Gets the value of the replyResp property.
     * 
     */
    public int getReplyResp() {
        return replyResp;
    }

    /**
     * Sets the value of the replyResp property.
     * 
     */
    public void setReplyResp(int value) {
        this.replyResp = value;
    }

    /**
     * Gets the value of the replyResp2 property.
     * 
     */
    public int getReplyResp2() {
        return replyResp2;
    }

    /**
     * Sets the value of the replyResp2 property.
     * 
     */
    public void setReplyResp2(int value) {
        this.replyResp2 = value;
    }

    /**
     * Gets the value of the replyMessage property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getReplyMessage() {
        return replyMessage;
    }

    /**
     * Sets the value of the replyMessage property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setReplyMessage(String value) {
        this.replyMessage = value;
    }

}

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
 * <p>Java class for ReplyItemType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ReplyItemType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ReplyNumber" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="ReplyPersonal" type="{http://legstar.com/test/coxb/lsfileal}ReplyPersonalType"/>
 *         &lt;element name="ReplyDate" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="ReplyAmount" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="ReplyComment" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ReplyItemType", propOrder = {
    "replyNumber",
    "replyPersonal",
    "replyDate",
    "replyAmount",
    "replyComment"
})
public class ReplyItemType {

    @XmlElement(name = "ReplyNumber")
    protected long replyNumber;
    @XmlElement(name = "ReplyPersonal", required = true)
    protected ReplyPersonalType replyPersonal;
    @XmlElement(name = "ReplyDate", required = true)
    protected String replyDate;
    @XmlElement(name = "ReplyAmount", required = true)
    protected String replyAmount;
    @XmlElement(name = "ReplyComment", required = true)
    protected String replyComment;

    /**
     * Gets the value of the replyNumber property.
     * 
     */
    public long getReplyNumber() {
        return replyNumber;
    }

    /**
     * Sets the value of the replyNumber property.
     * 
     */
    public void setReplyNumber(long value) {
        this.replyNumber = value;
    }

    /**
     * Gets the value of the replyPersonal property.
     * 
     * @return
     *     possible object is
     *     {@link ReplyPersonalType }
     *     
     */
    public ReplyPersonalType getReplyPersonal() {
        return replyPersonal;
    }

    /**
     * Sets the value of the replyPersonal property.
     * 
     * @param value
     *     allowed object is
     *     {@link ReplyPersonalType }
     *     
     */
    public void setReplyPersonal(ReplyPersonalType value) {
        this.replyPersonal = value;
    }

    /**
     * Gets the value of the replyDate property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getReplyDate() {
        return replyDate;
    }

    /**
     * Sets the value of the replyDate property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setReplyDate(String value) {
        this.replyDate = value;
    }

    /**
     * Gets the value of the replyAmount property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getReplyAmount() {
        return replyAmount;
    }

    /**
     * Sets the value of the replyAmount property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setReplyAmount(String value) {
        this.replyAmount = value;
    }

    /**
     * Gets the value of the replyComment property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getReplyComment() {
        return replyComment;
    }

    /**
     * Sets the value of the replyComment property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setReplyComment(String value) {
        this.replyComment = value;
    }

}

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

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for Filler65Type complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Filler65Type">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ReplyItemscount" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="ReplyItem" type="{http://legstar.com/test/coxb/lsfileal}ReplyItemType" maxOccurs="unbounded"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Filler65Type", propOrder = {
    "replyItemscount",
    "replyItem"
})
public class Filler65Type {

    @XmlElement(name = "ReplyItemscount")
    protected long replyItemscount;
    @XmlElement(name = "ReplyItem", required = true)
    protected List<ReplyItemType> replyItem;

    /**
     * Gets the value of the replyItemscount property.
     * 
     */
    public long getReplyItemscount() {
        return replyItemscount;
    }

    /**
     * Sets the value of the replyItemscount property.
     * 
     */
    public void setReplyItemscount(long value) {
        this.replyItemscount = value;
    }

    /**
     * Gets the value of the replyItem property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the replyItem property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getReplyItem().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ReplyItemType }
     * 
     * 
     */
    public List<ReplyItemType> getReplyItem() {
        if (replyItem == null) {
            replyItem = new ArrayList<ReplyItemType>();
        }
        return this.replyItem;
    }

}

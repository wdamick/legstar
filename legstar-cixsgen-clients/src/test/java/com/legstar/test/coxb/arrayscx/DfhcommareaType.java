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

package com.legstar.test.coxb.arrayscx;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for DfhcommareaType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="DfhcommareaType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="TableThree" type="{http://legstar.com/test/coxb/arrayscx}TableThreeType"/>
 *         &lt;element name="TableRedef" type="{http://legstar.com/test/coxb/arrayscx}TableRedefType" maxOccurs="unbounded"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "DfhcommareaType", propOrder = {
    "tableThree",
    "tableRedef"
})
public class DfhcommareaType {

    @XmlElement(name = "TableThree", required = true)
    protected TableThreeType tableThree;
    @XmlElement(name = "TableRedef", required = true)
    protected List<TableRedefType> tableRedef;

    /**
     * Gets the value of the tableThree property.
     * 
     * @return
     *     possible object is
     *     {@link TableThreeType }
     *     
     */
    public TableThreeType getTableThree() {
        return tableThree;
    }

    /**
     * Sets the value of the tableThree property.
     * 
     * @param value
     *     allowed object is
     *     {@link TableThreeType }
     *     
     */
    public void setTableThree(TableThreeType value) {
        this.tableThree = value;
    }

    /**
     * Gets the value of the tableRedef property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the tableRedef property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getTableRedef().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link TableRedefType }
     * 
     * 
     */
    public List<TableRedefType> getTableRedef() {
        if (tableRedef == null) {
            tableRedef = new ArrayList<TableRedefType>();
        }
        return this.tableRedef;
    }

}

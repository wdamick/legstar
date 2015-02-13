/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.test.coxb.issue161;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for Dfhcommarea complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Dfhcommarea">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="occursCounters" type="{http://coxb.test.legstar.com/issue161}OccursCounters"/>
 *         &lt;element name="occursTables" type="{http://coxb.test.legstar.com/issue161}OccursTables"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Dfhcommarea", propOrder = {
    "occursCounters",
    "occursTables"
})
public class Dfhcommarea
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(required = true)
    @CobolElement(cobolName = "OCCURS-COUNTERS", type = CobolType.GROUP_ITEM, levelNumber = 5, srceLine = 2)
    protected OccursCounters occursCounters;
    @XmlElement(required = true)
    @CobolElement(cobolName = "OCCURS-TABLES", type = CobolType.GROUP_ITEM, levelNumber = 5, srceLine = 4)
    protected OccursTables occursTables;

    /**
     * Gets the value of the occursCounters property.
     * 
     * @return
     *     possible object is
     *     {@link OccursCounters }
     *     
     */
    public OccursCounters getOccursCounters() {
        return occursCounters;
    }

    /**
     * Sets the value of the occursCounters property.
     * 
     * @param value
     *     allowed object is
     *     {@link OccursCounters }
     *     
     */
    public void setOccursCounters(OccursCounters value) {
        this.occursCounters = value;
    }

    public boolean isSetOccursCounters() {
        return (this.occursCounters!= null);
    }

    /**
     * Gets the value of the occursTables property.
     * 
     * @return
     *     possible object is
     *     {@link OccursTables }
     *     
     */
    public OccursTables getOccursTables() {
        return occursTables;
    }

    /**
     * Sets the value of the occursTables property.
     * 
     * @param value
     *     allowed object is
     *     {@link OccursTables }
     *     
     */
    public void setOccursTables(OccursTables value) {
        this.occursTables = value;
    }

    public boolean isSetOccursTables() {
        return (this.occursTables!= null);
    }

}

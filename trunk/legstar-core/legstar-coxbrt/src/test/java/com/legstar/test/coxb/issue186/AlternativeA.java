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
package com.legstar.test.coxb.issue186;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;

/**
 * <p>
 * Java class for AlternativeA complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="AlternativeA">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="OdoCounter">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *               &lt;totalDigits value="4"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "AlternativeA", propOrder = { "odoCounter" })
public class AlternativeA implements Serializable {

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "OdoCounter")
    @CobolElement(cobolName = "ODO-COUNTER", type = CobolType.BINARY_ITEM, levelNumber = 5, isSigned = false, totalDigits = 4, isODOObject = true, picture = "9(4)", usage = "BINARY", srceLine = 6)
    protected int odoCounter;

    /**
     * Gets the value of the odoCounter property.
     * 
     */
    public int getOdoCounter() {
        return odoCounter;
    }

    /**
     * Sets the value of the odoCounter property.
     * 
     */
    public void setOdoCounter(int value) {
        this.odoCounter = value;
    }

    public boolean isSetOdoCounter() {
        return true;
    }

}

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
package com.legstar.test.coxb.issue162;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for ConfprtyFrmtr complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ConfprtyFrmtr">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="confprtyFrmtrData">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="42"/>
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
@XmlType(name = "ConfprtyFrmtr", propOrder = {
    "confprtyFrmtrData"
})
public class ConfprtyFrmtr
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(required = true)
    @CobolElement(cobolName = "CONFPRTY-FRMTR-DATA", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 20, picture = "X(42)", srceLine = 6)
    protected String confprtyFrmtrData;

    /**
     * Gets the value of the confprtyFrmtrData property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getConfprtyFrmtrData() {
        return confprtyFrmtrData;
    }

    /**
     * Sets the value of the confprtyFrmtrData property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setConfprtyFrmtrData(String value) {
        this.confprtyFrmtrData = value;
    }

    public boolean isSetConfprtyFrmtrData() {
        return (this.confprtyFrmtrData!= null);
    }

}

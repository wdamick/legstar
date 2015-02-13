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
 * <p>Java class for ConfprtyFrmtq complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ConfprtyFrmtq">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="confprtyFrmtqData">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="140"/>
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
@XmlType(name = "ConfprtyFrmtq", propOrder = {
    "confprtyFrmtqData"
})
public class ConfprtyFrmtq
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(required = true)
    @CobolElement(cobolName = "CONFPRTY-FRMTQ-DATA", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 20, picture = "X(140)", srceLine = 4)
    protected String confprtyFrmtqData;

    /**
     * Gets the value of the confprtyFrmtqData property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getConfprtyFrmtqData() {
        return confprtyFrmtqData;
    }

    /**
     * Sets the value of the confprtyFrmtqData property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setConfprtyFrmtqData(String value) {
        this.confprtyFrmtqData = value;
    }

    public boolean isSetConfprtyFrmtqData() {
        return (this.confprtyFrmtqData!= null);
    }

}

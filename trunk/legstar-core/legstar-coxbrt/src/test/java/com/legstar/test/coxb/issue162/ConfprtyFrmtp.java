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
 * <p>Java class for ConfprtyFrmtp complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ConfprtyFrmtp">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="confprtyFrmtpData">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="11"/>
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
@XmlType(name = "ConfprtyFrmtp", propOrder = {
    "confprtyFrmtpData"
})
public class ConfprtyFrmtp
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(required = true)
    @CobolElement(cobolName = "CONFPRTY-FRMTP-DATA", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 20, picture = "X(11)", srceLine = 8)
    protected String confprtyFrmtpData;

    /**
     * Gets the value of the confprtyFrmtpData property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getConfprtyFrmtpData() {
        return confprtyFrmtpData;
    }

    /**
     * Sets the value of the confprtyFrmtpData property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setConfprtyFrmtpData(String value) {
        this.confprtyFrmtpData = value;
    }

    public boolean isSetConfprtyFrmtpData() {
        return (this.confprtyFrmtpData!= null);
    }

}

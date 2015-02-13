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
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for Confprty complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Confprty">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;choice>
 *           &lt;element name="confprtyFrmtq" type="{http://coxb.test.legstar.com/issue162}ConfprtyFrmtq"/>
 *           &lt;element name="confprtyFrmtr" type="{http://coxb.test.legstar.com/issue162}ConfprtyFrmtr"/>
 *           &lt;element name="confprtyFrmtp" type="{http://coxb.test.legstar.com/issue162}ConfprtyFrmtp"/>
 *         &lt;/choice>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Confprty", propOrder = {
    "confprtyFrmtq",
    "confprtyFrmtr",
    "confprtyFrmtp"
})
public class Confprty
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @CobolElement(cobolName = "CONFPRTY-FRMTQ", type = CobolType.GROUP_ITEM, levelNumber = 15, isRedefined = true, srceLine = 3)
    protected ConfprtyFrmtq confprtyFrmtq;
    @CobolElement(cobolName = "CONFPRTY-FRMTR", type = CobolType.GROUP_ITEM, levelNumber = 15, redefines = "CONFPRTY-FRMTQ", srceLine = 5)
    protected ConfprtyFrmtr confprtyFrmtr;
    @CobolElement(cobolName = "CONFPRTY-FRMTP", type = CobolType.GROUP_ITEM, levelNumber = 15, redefines = "CONFPRTY-FRMTQ", srceLine = 7)
    protected ConfprtyFrmtp confprtyFrmtp;

    /**
     * Gets the value of the confprtyFrmtq property.
     * 
     * @return
     *     possible object is
     *     {@link ConfprtyFrmtq }
     *     
     */
    public ConfprtyFrmtq getConfprtyFrmtq() {
        return confprtyFrmtq;
    }

    /**
     * Sets the value of the confprtyFrmtq property.
     * 
     * @param value
     *     allowed object is
     *     {@link ConfprtyFrmtq }
     *     
     */
    public void setConfprtyFrmtq(ConfprtyFrmtq value) {
        this.confprtyFrmtq = value;
    }

    public boolean isSetConfprtyFrmtq() {
        return (this.confprtyFrmtq!= null);
    }

    /**
     * Gets the value of the confprtyFrmtr property.
     * 
     * @return
     *     possible object is
     *     {@link ConfprtyFrmtr }
     *     
     */
    public ConfprtyFrmtr getConfprtyFrmtr() {
        return confprtyFrmtr;
    }

    /**
     * Sets the value of the confprtyFrmtr property.
     * 
     * @param value
     *     allowed object is
     *     {@link ConfprtyFrmtr }
     *     
     */
    public void setConfprtyFrmtr(ConfprtyFrmtr value) {
        this.confprtyFrmtr = value;
    }

    public boolean isSetConfprtyFrmtr() {
        return (this.confprtyFrmtr!= null);
    }

    /**
     * Gets the value of the confprtyFrmtp property.
     * 
     * @return
     *     possible object is
     *     {@link ConfprtyFrmtp }
     *     
     */
    public ConfprtyFrmtp getConfprtyFrmtp() {
        return confprtyFrmtp;
    }

    /**
     * Sets the value of the confprtyFrmtp property.
     * 
     * @param value
     *     allowed object is
     *     {@link ConfprtyFrmtp }
     *     
     */
    public void setConfprtyFrmtp(ConfprtyFrmtp value) {
        this.confprtyFrmtp = value;
    }

    public boolean isSetConfprtyFrmtp() {
        return (this.confprtyFrmtp!= null);
    }

}

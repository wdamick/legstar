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
package com.legstar.test.coxb.issue185;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for Commarea complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Commarea">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;choice>
 *           &lt;element name="outerRedefinesLong">
 *             &lt;simpleType>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *                 &lt;maxLength value="10"/>
 *               &lt;/restriction>
 *             &lt;/simpleType>
 *           &lt;/element>
 *           &lt;element name="outerRedefinesShort" type="{http://coxb.test.legstar.com/issue185}OuterRedefinesShort"/>
 *         &lt;/choice>
 *         &lt;element name="footer">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="1"/>
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
@XmlType(name = "Commarea", propOrder = {
    "outerRedefinesLong",
    "outerRedefinesShort",
    "footer"
})
public class Commarea
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @CobolElement(cobolName = "OUTER-REDEFINES-LONG", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 10, isRedefined = true, picture = "X(10)", srceLine = 2)
    protected String outerRedefinesLong;
    @CobolElement(cobolName = "OUTER-REDEFINES-SHORT", type = CobolType.GROUP_ITEM, levelNumber = 10, redefines = "OUTER-REDEFINES-LONG", srceLine = 3)
    protected OuterRedefinesShort outerRedefinesShort;
    @XmlElement(required = true)
    @CobolElement(cobolName = "FOOTER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 10, picture = "X", srceLine = 8)
    protected String footer;

    /**
     * Gets the value of the outerRedefinesLong property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getOuterRedefinesLong() {
        return outerRedefinesLong;
    }

    /**
     * Sets the value of the outerRedefinesLong property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setOuterRedefinesLong(String value) {
        this.outerRedefinesLong = value;
    }

    public boolean isSetOuterRedefinesLong() {
        return (this.outerRedefinesLong!= null);
    }

    /**
     * Gets the value of the outerRedefinesShort property.
     * 
     * @return
     *     possible object is
     *     {@link OuterRedefinesShort }
     *     
     */
    public OuterRedefinesShort getOuterRedefinesShort() {
        return outerRedefinesShort;
    }

    /**
     * Sets the value of the outerRedefinesShort property.
     * 
     * @param value
     *     allowed object is
     *     {@link OuterRedefinesShort }
     *     
     */
    public void setOuterRedefinesShort(OuterRedefinesShort value) {
        this.outerRedefinesShort = value;
    }

    public boolean isSetOuterRedefinesShort() {
        return (this.outerRedefinesShort!= null);
    }

    /**
     * Gets the value of the footer property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFooter() {
        return footer;
    }

    /**
     * Sets the value of the footer property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFooter(String value) {
        this.footer = value;
    }

    public boolean isSetFooter() {
        return (this.footer!= null);
    }

}

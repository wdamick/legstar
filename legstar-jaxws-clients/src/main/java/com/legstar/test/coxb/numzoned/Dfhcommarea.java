
package com.legstar.test.coxb.numzoned;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


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
 *         &lt;element name="LU" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="LS" type="{http://www.w3.org/2001/XMLSchema}short"/>
 *         &lt;element name="LSSignL" type="{http://www.w3.org/2001/XMLSchema}short"/>
 *         &lt;element name="LSSignT" type="{http://www.w3.org/2001/XMLSchema}short"/>
 *         &lt;element name="LSSignSL" type="{http://www.w3.org/2001/XMLSchema}short"/>
 *         &lt;element name="LSSignST" type="{http://www.w3.org/2001/XMLSchema}short"/>
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
    "lu",
    "ls",
    "lsSignL",
    "lsSignT",
    "lsSignSL",
    "lsSignST"
})
public class Dfhcommarea {

    @XmlElement(name = "LU")
    protected int lu;
    @XmlElement(name = "LS")
    protected short ls;
    @XmlElement(name = "LSSignL")
    protected short lsSignL;
    @XmlElement(name = "LSSignT")
    protected short lsSignT;
    @XmlElement(name = "LSSignSL")
    protected short lsSignSL;
    @XmlElement(name = "LSSignST")
    protected short lsSignST;

    /**
     * Gets the value of the lu property.
     * 
     */
    public int getLU() {
        return lu;
    }

    /**
     * Sets the value of the lu property.
     * 
     */
    public void setLU(int value) {
        this.lu = value;
    }

    /**
     * Gets the value of the ls property.
     * 
     */
    public short getLS() {
        return ls;
    }

    /**
     * Sets the value of the ls property.
     * 
     */
    public void setLS(short value) {
        this.ls = value;
    }

    /**
     * Gets the value of the lsSignL property.
     * 
     */
    public short getLSSignL() {
        return lsSignL;
    }

    /**
     * Sets the value of the lsSignL property.
     * 
     */
    public void setLSSignL(short value) {
        this.lsSignL = value;
    }

    /**
     * Gets the value of the lsSignT property.
     * 
     */
    public short getLSSignT() {
        return lsSignT;
    }

    /**
     * Sets the value of the lsSignT property.
     * 
     */
    public void setLSSignT(short value) {
        this.lsSignT = value;
    }

    /**
     * Gets the value of the lsSignSL property.
     * 
     */
    public short getLSSignSL() {
        return lsSignSL;
    }

    /**
     * Sets the value of the lsSignSL property.
     * 
     */
    public void setLSSignSL(short value) {
        this.lsSignSL = value;
    }

    /**
     * Gets the value of the lsSignST property.
     * 
     */
    public short getLSSignST() {
        return lsSignST;
    }

    /**
     * Sets the value of the lsSignST property.
     * 
     */
    public void setLSSignST(short value) {
        this.lsSignST = value;
    }

}

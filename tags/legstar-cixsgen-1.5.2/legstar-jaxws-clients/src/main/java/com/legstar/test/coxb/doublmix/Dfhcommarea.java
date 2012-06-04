
package com.legstar.test.coxb.doublmix;

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
 *         &lt;element name="CDouble1234" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="CDouble0" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="CDouble1" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="CDouble345006P5678" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="CDouble798P20067Em16" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="CDouble3P40282347Ep38" type="{http://www.w3.org/2001/XMLSchema}double"/>
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
    "cDouble1234",
    "cDouble0",
    "cDouble1",
    "cDouble345006P5678",
    "cDouble798P20067Em16",
    "cDouble3P40282347Ep38"
})
public class Dfhcommarea {

    @XmlElement(name = "CDouble1234")
    protected double cDouble1234;
    @XmlElement(name = "CDouble0")
    protected double cDouble0;
    @XmlElement(name = "CDouble1")
    protected double cDouble1;
    @XmlElement(name = "CDouble345006P5678")
    protected double cDouble345006P5678;
    @XmlElement(name = "CDouble798P20067Em16")
    protected double cDouble798P20067Em16;
    @XmlElement(name = "CDouble3P40282347Ep38")
    protected double cDouble3P40282347Ep38;

    /**
     * Gets the value of the cDouble1234 property.
     * 
     */
    public double getCDouble1234() {
        return cDouble1234;
    }

    /**
     * Sets the value of the cDouble1234 property.
     * 
     */
    public void setCDouble1234(double value) {
        this.cDouble1234 = value;
    }

    /**
     * Gets the value of the cDouble0 property.
     * 
     */
    public double getCDouble0() {
        return cDouble0;
    }

    /**
     * Sets the value of the cDouble0 property.
     * 
     */
    public void setCDouble0(double value) {
        this.cDouble0 = value;
    }

    /**
     * Gets the value of the cDouble1 property.
     * 
     */
    public double getCDouble1() {
        return cDouble1;
    }

    /**
     * Sets the value of the cDouble1 property.
     * 
     */
    public void setCDouble1(double value) {
        this.cDouble1 = value;
    }

    /**
     * Gets the value of the cDouble345006P5678 property.
     * 
     */
    public double getCDouble345006P5678() {
        return cDouble345006P5678;
    }

    /**
     * Sets the value of the cDouble345006P5678 property.
     * 
     */
    public void setCDouble345006P5678(double value) {
        this.cDouble345006P5678 = value;
    }

    /**
     * Gets the value of the cDouble798P20067Em16 property.
     * 
     */
    public double getCDouble798P20067Em16() {
        return cDouble798P20067Em16;
    }

    /**
     * Sets the value of the cDouble798P20067Em16 property.
     * 
     */
    public void setCDouble798P20067Em16(double value) {
        this.cDouble798P20067Em16 = value;
    }

    /**
     * Gets the value of the cDouble3P40282347Ep38 property.
     * 
     */
    public double getCDouble3P40282347Ep38() {
        return cDouble3P40282347Ep38;
    }

    /**
     * Sets the value of the cDouble3P40282347Ep38 property.
     * 
     */
    public void setCDouble3P40282347Ep38(double value) {
        this.cDouble3P40282347Ep38 = value;
    }

}

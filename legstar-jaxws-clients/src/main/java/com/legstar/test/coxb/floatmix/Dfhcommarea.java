
package com.legstar.test.coxb.floatmix;

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
 *         &lt;element name="CFloat1234" type="{http://www.w3.org/2001/XMLSchema}float"/>
 *         &lt;element name="CFloat0" type="{http://www.w3.org/2001/XMLSchema}float"/>
 *         &lt;element name="CFloat1" type="{http://www.w3.org/2001/XMLSchema}float"/>
 *         &lt;element name="CFloat345006P5678" type="{http://www.w3.org/2001/XMLSchema}float"/>
 *         &lt;element name="CFloat798P20067Em16" type="{http://www.w3.org/2001/XMLSchema}float"/>
 *         &lt;element name="CFloat3P40282347Ep38" type="{http://www.w3.org/2001/XMLSchema}float"/>
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
    "cFloat1234",
    "cFloat0",
    "cFloat1",
    "cFloat345006P5678",
    "cFloat798P20067Em16",
    "cFloat3P40282347Ep38"
})
public class Dfhcommarea {

    @XmlElement(name = "CFloat1234")
    protected float cFloat1234;
    @XmlElement(name = "CFloat0")
    protected float cFloat0;
    @XmlElement(name = "CFloat1")
    protected float cFloat1;
    @XmlElement(name = "CFloat345006P5678")
    protected float cFloat345006P5678;
    @XmlElement(name = "CFloat798P20067Em16")
    protected float cFloat798P20067Em16;
    @XmlElement(name = "CFloat3P40282347Ep38")
    protected float cFloat3P40282347Ep38;

    /**
     * Gets the value of the cFloat1234 property.
     * 
     */
    public float getCFloat1234() {
        return cFloat1234;
    }

    /**
     * Sets the value of the cFloat1234 property.
     * 
     */
    public void setCFloat1234(float value) {
        this.cFloat1234 = value;
    }

    /**
     * Gets the value of the cFloat0 property.
     * 
     */
    public float getCFloat0() {
        return cFloat0;
    }

    /**
     * Sets the value of the cFloat0 property.
     * 
     */
    public void setCFloat0(float value) {
        this.cFloat0 = value;
    }

    /**
     * Gets the value of the cFloat1 property.
     * 
     */
    public float getCFloat1() {
        return cFloat1;
    }

    /**
     * Sets the value of the cFloat1 property.
     * 
     */
    public void setCFloat1(float value) {
        this.cFloat1 = value;
    }

    /**
     * Gets the value of the cFloat345006P5678 property.
     * 
     */
    public float getCFloat345006P5678() {
        return cFloat345006P5678;
    }

    /**
     * Sets the value of the cFloat345006P5678 property.
     * 
     */
    public void setCFloat345006P5678(float value) {
        this.cFloat345006P5678 = value;
    }

    /**
     * Gets the value of the cFloat798P20067Em16 property.
     * 
     */
    public float getCFloat798P20067Em16() {
        return cFloat798P20067Em16;
    }

    /**
     * Sets the value of the cFloat798P20067Em16 property.
     * 
     */
    public void setCFloat798P20067Em16(float value) {
        this.cFloat798P20067Em16 = value;
    }

    /**
     * Gets the value of the cFloat3P40282347Ep38 property.
     * 
     */
    public float getCFloat3P40282347Ep38() {
        return cFloat3P40282347Ep38;
    }

    /**
     * Sets the value of the cFloat3P40282347Ep38 property.
     * 
     */
    public void setCFloat3P40282347Ep38(float value) {
        this.cFloat3P40282347Ep38 = value;
    }

}

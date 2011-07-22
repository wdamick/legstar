
package com.legstar.test.coxb.fixarnum;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
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
 *         &lt;element name="CArrayPd" type="{http://www.w3.org/2001/XMLSchema}decimal" maxOccurs="unbounded"/>
 *         &lt;element name="CArrayZd" type="{http://www.w3.org/2001/XMLSchema}decimal" maxOccurs="unbounded"/>
 *         &lt;element name="CArrayZi" type="{http://www.w3.org/2001/XMLSchema}int" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="CArrayBi" type="{http://www.w3.org/2001/XMLSchema}long" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="CArrayNi" type="{http://www.w3.org/2001/XMLSchema}integer" maxOccurs="unbounded"/>
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
    "cArrayPd",
    "cArrayZd",
    "cArrayZi",
    "cArrayBi",
    "cArrayNi"
})
public class Dfhcommarea {

    @XmlElement(name = "CArrayPd", required = true)
    protected List<BigDecimal> cArrayPd;
    @XmlElement(name = "CArrayZd", required = true)
    protected List<BigDecimal> cArrayZd;
    @XmlElement(name = "CArrayZi", type = Integer.class)
    protected List<Integer> cArrayZi;
    @XmlElement(name = "CArrayBi", type = Long.class)
    protected List<Long> cArrayBi;
    @XmlElement(name = "CArrayNi", required = true)
    protected List<BigInteger> cArrayNi;

    /**
     * Gets the value of the cArrayPd property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the cArrayPd property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getCArrayPd().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link BigDecimal }
     * 
     * 
     */
    public List<BigDecimal> getCArrayPd() {
        if (cArrayPd == null) {
            cArrayPd = new ArrayList<BigDecimal>();
        }
        return this.cArrayPd;
    }

    /**
     * Gets the value of the cArrayZd property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the cArrayZd property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getCArrayZd().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link BigDecimal }
     * 
     * 
     */
    public List<BigDecimal> getCArrayZd() {
        if (cArrayZd == null) {
            cArrayZd = new ArrayList<BigDecimal>();
        }
        return this.cArrayZd;
    }

    /**
     * Gets the value of the cArrayZi property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the cArrayZi property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getCArrayZi().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Integer }
     * 
     * 
     */
    public List<Integer> getCArrayZi() {
        if (cArrayZi == null) {
            cArrayZi = new ArrayList<Integer>();
        }
        return this.cArrayZi;
    }

    /**
     * Gets the value of the cArrayBi property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the cArrayBi property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getCArrayBi().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Long }
     * 
     * 
     */
    public List<Long> getCArrayBi() {
        if (cArrayBi == null) {
            cArrayBi = new ArrayList<Long>();
        }
        return this.cArrayBi;
    }

    /**
     * Gets the value of the cArrayNi property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the cArrayNi property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getCArrayNi().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link BigInteger }
     * 
     * 
     */
    public List<BigInteger> getCArrayNi() {
        if (cArrayNi == null) {
            cArrayNi = new ArrayList<BigInteger>();
        }
        return this.cArrayNi;
    }

}

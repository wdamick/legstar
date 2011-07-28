
package com.legstar.test.coxb.alltypes;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.HexBinaryAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;


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
 *         &lt;element name="SString" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="SBinary" type="{http://www.w3.org/2001/XMLSchema}hexBinary"/>
 *         &lt;element name="SShort" type="{http://www.w3.org/2001/XMLSchema}short"/>
 *         &lt;element name="SUshort" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="SInt" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="SUint" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="SLong" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="SUlong" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="SXlong" type="{http://www.w3.org/2001/XMLSchema}integer"/>
 *         &lt;element name="SUxlong" type="{http://www.w3.org/2001/XMLSchema}integer"/>
 *         &lt;element name="SDec" type="{http://www.w3.org/2001/XMLSchema}decimal"/>
 *         &lt;element name="SFloat" type="{http://www.w3.org/2001/XMLSchema}float"/>
 *         &lt;element name="SDouble" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="AString" type="{http://www.w3.org/2001/XMLSchema}string" maxOccurs="unbounded"/>
 *         &lt;element name="ABinary" type="{http://www.w3.org/2001/XMLSchema}string" maxOccurs="unbounded"/>
 *         &lt;element name="AShort" type="{http://www.w3.org/2001/XMLSchema}short" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="AUshort" type="{http://www.w3.org/2001/XMLSchema}int" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="AInt" type="{http://www.w3.org/2001/XMLSchema}int" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="AUint" type="{http://www.w3.org/2001/XMLSchema}long" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="ALong" type="{http://www.w3.org/2001/XMLSchema}long" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="AUlong" type="{http://www.w3.org/2001/XMLSchema}long" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="AXlong" type="{http://www.w3.org/2001/XMLSchema}integer" maxOccurs="unbounded"/>
 *         &lt;element name="AUxlong" type="{http://www.w3.org/2001/XMLSchema}integer" maxOccurs="unbounded"/>
 *         &lt;element name="ADec" type="{http://www.w3.org/2001/XMLSchema}decimal" maxOccurs="unbounded"/>
 *         &lt;element name="AFloat" type="{http://www.w3.org/2001/XMLSchema}float" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="ADouble" type="{http://www.w3.org/2001/XMLSchema}double" maxOccurs="unbounded" minOccurs="0"/>
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
    "sString",
    "sBinary",
    "sShort",
    "sUshort",
    "sInt",
    "sUint",
    "sLong",
    "sUlong",
    "sXlong",
    "sUxlong",
    "sDec",
    "sFloat",
    "sDouble",
    "aString",
    "aBinary",
    "aShort",
    "aUshort",
    "aInt",
    "aUint",
    "aLong",
    "aUlong",
    "aXlong",
    "aUxlong",
    "aDec",
    "aFloat",
    "aDouble"
})
public class Dfhcommarea {

    @XmlElement(name = "SString", required = true)
    protected String sString;
    @XmlElement(name = "SBinary", required = true, type = String.class)
    @XmlJavaTypeAdapter(HexBinaryAdapter.class)
    @XmlSchemaType(name = "hexBinary")
    protected byte[] sBinary;
    @XmlElement(name = "SShort")
    protected short sShort;
    @XmlElement(name = "SUshort")
    protected int sUshort;
    @XmlElement(name = "SInt")
    protected int sInt;
    @XmlElement(name = "SUint")
    protected long sUint;
    @XmlElement(name = "SLong")
    protected long sLong;
    @XmlElement(name = "SUlong")
    protected long sUlong;
    @XmlElement(name = "SXlong", required = true)
    protected BigInteger sXlong;
    @XmlElement(name = "SUxlong", required = true)
    protected BigInteger sUxlong;
    @XmlElement(name = "SDec", required = true)
    protected BigDecimal sDec;
    @XmlElement(name = "SFloat")
    protected float sFloat;
    @XmlElement(name = "SDouble")
    protected double sDouble;
    @XmlElement(name = "AString", required = true)
    protected List<String> aString;
    @XmlElement(name = "ABinary", required = true)
    protected List<String> aBinary;
    @XmlElement(name = "AShort", type = Short.class)
    protected List<Short> aShort;
    @XmlElement(name = "AUshort", type = Integer.class)
    protected List<Integer> aUshort;
    @XmlElement(name = "AInt", type = Integer.class)
    protected List<Integer> aInt;
    @XmlElement(name = "AUint", type = Long.class)
    protected List<Long> aUint;
    @XmlElement(name = "ALong", type = Long.class)
    protected List<Long> aLong;
    @XmlElement(name = "AUlong", type = Long.class)
    protected List<Long> aUlong;
    @XmlElement(name = "AXlong", required = true)
    protected List<BigInteger> aXlong;
    @XmlElement(name = "AUxlong", required = true)
    protected List<BigInteger> aUxlong;
    @XmlElement(name = "ADec", required = true)
    protected List<BigDecimal> aDec;
    @XmlElement(name = "AFloat", type = Float.class)
    protected List<Float> aFloat;
    @XmlElement(name = "ADouble", type = Double.class)
    protected List<Double> aDouble;

    /**
     * Gets the value of the sString property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSString() {
        return sString;
    }

    /**
     * Sets the value of the sString property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSString(String value) {
        this.sString = value;
    }

    /**
     * Gets the value of the sBinary property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public byte[] getSBinary() {
        return sBinary;
    }

    /**
     * Sets the value of the sBinary property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSBinary(byte[] value) {
        this.sBinary = ((byte[]) value);
    }

    /**
     * Gets the value of the sShort property.
     * 
     */
    public short getSShort() {
        return sShort;
    }

    /**
     * Sets the value of the sShort property.
     * 
     */
    public void setSShort(short value) {
        this.sShort = value;
    }

    /**
     * Gets the value of the sUshort property.
     * 
     */
    public int getSUshort() {
        return sUshort;
    }

    /**
     * Sets the value of the sUshort property.
     * 
     */
    public void setSUshort(int value) {
        this.sUshort = value;
    }

    /**
     * Gets the value of the sInt property.
     * 
     */
    public int getSInt() {
        return sInt;
    }

    /**
     * Sets the value of the sInt property.
     * 
     */
    public void setSInt(int value) {
        this.sInt = value;
    }

    /**
     * Gets the value of the sUint property.
     * 
     */
    public long getSUint() {
        return sUint;
    }

    /**
     * Sets the value of the sUint property.
     * 
     */
    public void setSUint(long value) {
        this.sUint = value;
    }

    /**
     * Gets the value of the sLong property.
     * 
     */
    public long getSLong() {
        return sLong;
    }

    /**
     * Sets the value of the sLong property.
     * 
     */
    public void setSLong(long value) {
        this.sLong = value;
    }

    /**
     * Gets the value of the sUlong property.
     * 
     */
    public long getSUlong() {
        return sUlong;
    }

    /**
     * Sets the value of the sUlong property.
     * 
     */
    public void setSUlong(long value) {
        this.sUlong = value;
    }

    /**
     * Gets the value of the sXlong property.
     * 
     * @return
     *     possible object is
     *     {@link BigInteger }
     *     
     */
    public BigInteger getSXlong() {
        return sXlong;
    }

    /**
     * Sets the value of the sXlong property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigInteger }
     *     
     */
    public void setSXlong(BigInteger value) {
        this.sXlong = value;
    }

    /**
     * Gets the value of the sUxlong property.
     * 
     * @return
     *     possible object is
     *     {@link BigInteger }
     *     
     */
    public BigInteger getSUxlong() {
        return sUxlong;
    }

    /**
     * Sets the value of the sUxlong property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigInteger }
     *     
     */
    public void setSUxlong(BigInteger value) {
        this.sUxlong = value;
    }

    /**
     * Gets the value of the sDec property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getSDec() {
        return sDec;
    }

    /**
     * Sets the value of the sDec property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setSDec(BigDecimal value) {
        this.sDec = value;
    }

    /**
     * Gets the value of the sFloat property.
     * 
     */
    public float getSFloat() {
        return sFloat;
    }

    /**
     * Sets the value of the sFloat property.
     * 
     */
    public void setSFloat(float value) {
        this.sFloat = value;
    }

    /**
     * Gets the value of the sDouble property.
     * 
     */
    public double getSDouble() {
        return sDouble;
    }

    /**
     * Sets the value of the sDouble property.
     * 
     */
    public void setSDouble(double value) {
        this.sDouble = value;
    }

    /**
     * Gets the value of the aString property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the aString property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getAString().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link String }
     * 
     * 
     */
    public List<String> getAString() {
        if (aString == null) {
            aString = new ArrayList<String>();
        }
        return this.aString;
    }

    /**
     * Gets the value of the aBinary property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the aBinary property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getABinary().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link String }
     * 
     * 
     */
    public List<String> getABinary() {
        if (aBinary == null) {
            aBinary = new ArrayList<String>();
        }
        return this.aBinary;
    }

    /**
     * Gets the value of the aShort property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the aShort property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getAShort().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Short }
     * 
     * 
     */
    public List<Short> getAShort() {
        if (aShort == null) {
            aShort = new ArrayList<Short>();
        }
        return this.aShort;
    }

    /**
     * Gets the value of the aUshort property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the aUshort property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getAUshort().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Integer }
     * 
     * 
     */
    public List<Integer> getAUshort() {
        if (aUshort == null) {
            aUshort = new ArrayList<Integer>();
        }
        return this.aUshort;
    }

    /**
     * Gets the value of the aInt property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the aInt property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getAInt().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Integer }
     * 
     * 
     */
    public List<Integer> getAInt() {
        if (aInt == null) {
            aInt = new ArrayList<Integer>();
        }
        return this.aInt;
    }

    /**
     * Gets the value of the aUint property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the aUint property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getAUint().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Long }
     * 
     * 
     */
    public List<Long> getAUint() {
        if (aUint == null) {
            aUint = new ArrayList<Long>();
        }
        return this.aUint;
    }

    /**
     * Gets the value of the aLong property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the aLong property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getALong().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Long }
     * 
     * 
     */
    public List<Long> getALong() {
        if (aLong == null) {
            aLong = new ArrayList<Long>();
        }
        return this.aLong;
    }

    /**
     * Gets the value of the aUlong property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the aUlong property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getAUlong().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Long }
     * 
     * 
     */
    public List<Long> getAUlong() {
        if (aUlong == null) {
            aUlong = new ArrayList<Long>();
        }
        return this.aUlong;
    }

    /**
     * Gets the value of the aXlong property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the aXlong property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getAXlong().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link BigInteger }
     * 
     * 
     */
    public List<BigInteger> getAXlong() {
        if (aXlong == null) {
            aXlong = new ArrayList<BigInteger>();
        }
        return this.aXlong;
    }

    /**
     * Gets the value of the aUxlong property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the aUxlong property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getAUxlong().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link BigInteger }
     * 
     * 
     */
    public List<BigInteger> getAUxlong() {
        if (aUxlong == null) {
            aUxlong = new ArrayList<BigInteger>();
        }
        return this.aUxlong;
    }

    /**
     * Gets the value of the aDec property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the aDec property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getADec().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link BigDecimal }
     * 
     * 
     */
    public List<BigDecimal> getADec() {
        if (aDec == null) {
            aDec = new ArrayList<BigDecimal>();
        }
        return this.aDec;
    }

    /**
     * Gets the value of the aFloat property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the aFloat property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getAFloat().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Float }
     * 
     * 
     */
    public List<Float> getAFloat() {
        if (aFloat == null) {
            aFloat = new ArrayList<Float>();
        }
        return this.aFloat;
    }

    /**
     * Gets the value of the aDouble property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the aDouble property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getADouble().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Double }
     * 
     * 
     */
    public List<Double> getADouble() {
        if (aDouble == null) {
            aDouble = new ArrayList<Double>();
        }
        return this.aDouble;
    }

}

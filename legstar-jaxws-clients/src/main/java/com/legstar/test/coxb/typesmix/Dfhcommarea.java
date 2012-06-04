
package com.legstar.test.coxb.typesmix;

import java.math.BigDecimal;
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
 *         &lt;element name="CAlphabetic" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="CNational" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="CDbcs" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="CAlphanumericEdited" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="CAlphanumeric" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="COctetString" type="{http://www.w3.org/2001/XMLSchema}hexBinary"/>
 *         &lt;element name="CSingleFloat" type="{http://www.w3.org/2001/XMLSchema}float"/>
 *         &lt;element name="CDoubleFloat" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="CPackedDecimal" type="{http://www.w3.org/2001/XMLSchema}decimal"/>
 *         &lt;element name="CZonedDecimal" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *         &lt;element name="CNumericEdited1" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="CNumericEdited2" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="CNumericEdited3" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="CNumericEdited4" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="CIndex" type="{http://www.w3.org/2001/XMLSchema}hexBinary"/>
 *         &lt;element name="CPointer" type="{http://www.w3.org/2001/XMLSchema}hexBinary"/>
 *         &lt;element name="CProcPointer" type="{http://www.w3.org/2001/XMLSchema}hexBinary"/>
 *         &lt;element name="CFuncPointer" type="{http://www.w3.org/2001/XMLSchema}hexBinary"/>
 *         &lt;element name="CExternalFloating" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="CBinary" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="CNativeBinary" type="{http://www.w3.org/2001/XMLSchema}int"/>
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
    "cAlphabetic",
    "cNational",
    "cDbcs",
    "cAlphanumericEdited",
    "cAlphanumeric",
    "cOctetString",
    "cSingleFloat",
    "cDoubleFloat",
    "cPackedDecimal",
    "cZonedDecimal",
    "cNumericEdited1",
    "cNumericEdited2",
    "cNumericEdited3",
    "cNumericEdited4",
    "cIndex",
    "cPointer",
    "cProcPointer",
    "cFuncPointer",
    "cExternalFloating",
    "cBinary",
    "cNativeBinary"
})
public class Dfhcommarea {

    @XmlElement(name = "CAlphabetic", required = true)
    protected String cAlphabetic;
    @XmlElement(name = "CNational", required = true)
    protected String cNational;
    @XmlElement(name = "CDbcs", required = true)
    protected String cDbcs;
    @XmlElement(name = "CAlphanumericEdited", required = true)
    protected String cAlphanumericEdited;
    @XmlElement(name = "CAlphanumeric", required = true)
    protected String cAlphanumeric;
    @XmlElement(name = "COctetString", required = true, type = String.class)
    @XmlJavaTypeAdapter(HexBinaryAdapter.class)
    @XmlSchemaType(name = "hexBinary")
    protected byte[] cOctetString;
    @XmlElement(name = "CSingleFloat")
    protected float cSingleFloat;
    @XmlElement(name = "CDoubleFloat")
    protected double cDoubleFloat;
    @XmlElement(name = "CPackedDecimal", required = true)
    protected BigDecimal cPackedDecimal;
    @XmlElement(name = "CZonedDecimal")
    protected long cZonedDecimal;
    @XmlElement(name = "CNumericEdited1", required = true)
    protected String cNumericEdited1;
    @XmlElement(name = "CNumericEdited2", required = true)
    protected String cNumericEdited2;
    @XmlElement(name = "CNumericEdited3", required = true)
    protected String cNumericEdited3;
    @XmlElement(name = "CNumericEdited4", required = true)
    protected String cNumericEdited4;
    @XmlElement(name = "CIndex", required = true, type = String.class)
    @XmlJavaTypeAdapter(HexBinaryAdapter.class)
    @XmlSchemaType(name = "hexBinary")
    protected byte[] cIndex;
    @XmlElement(name = "CPointer", required = true, type = String.class)
    @XmlJavaTypeAdapter(HexBinaryAdapter.class)
    @XmlSchemaType(name = "hexBinary")
    protected byte[] cPointer;
    @XmlElement(name = "CProcPointer", required = true, type = String.class)
    @XmlJavaTypeAdapter(HexBinaryAdapter.class)
    @XmlSchemaType(name = "hexBinary")
    protected byte[] cProcPointer;
    @XmlElement(name = "CFuncPointer", required = true, type = String.class)
    @XmlJavaTypeAdapter(HexBinaryAdapter.class)
    @XmlSchemaType(name = "hexBinary")
    protected byte[] cFuncPointer;
    @XmlElement(name = "CExternalFloating", required = true)
    protected String cExternalFloating;
    @XmlElement(name = "CBinary")
    protected int cBinary;
    @XmlElement(name = "CNativeBinary")
    protected int cNativeBinary;

    /**
     * Gets the value of the cAlphabetic property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCAlphabetic() {
        return cAlphabetic;
    }

    /**
     * Sets the value of the cAlphabetic property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCAlphabetic(String value) {
        this.cAlphabetic = value;
    }

    /**
     * Gets the value of the cNational property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCNational() {
        return cNational;
    }

    /**
     * Sets the value of the cNational property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCNational(String value) {
        this.cNational = value;
    }

    /**
     * Gets the value of the cDbcs property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCDbcs() {
        return cDbcs;
    }

    /**
     * Sets the value of the cDbcs property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCDbcs(String value) {
        this.cDbcs = value;
    }

    /**
     * Gets the value of the cAlphanumericEdited property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCAlphanumericEdited() {
        return cAlphanumericEdited;
    }

    /**
     * Sets the value of the cAlphanumericEdited property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCAlphanumericEdited(String value) {
        this.cAlphanumericEdited = value;
    }

    /**
     * Gets the value of the cAlphanumeric property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCAlphanumeric() {
        return cAlphanumeric;
    }

    /**
     * Sets the value of the cAlphanumeric property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCAlphanumeric(String value) {
        this.cAlphanumeric = value;
    }

    /**
     * Gets the value of the cOctetString property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public byte[] getCOctetString() {
        return cOctetString;
    }

    /**
     * Sets the value of the cOctetString property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCOctetString(byte[] value) {
        this.cOctetString = ((byte[]) value);
    }

    /**
     * Gets the value of the cSingleFloat property.
     * 
     */
    public float getCSingleFloat() {
        return cSingleFloat;
    }

    /**
     * Sets the value of the cSingleFloat property.
     * 
     */
    public void setCSingleFloat(float value) {
        this.cSingleFloat = value;
    }

    /**
     * Gets the value of the cDoubleFloat property.
     * 
     */
    public double getCDoubleFloat() {
        return cDoubleFloat;
    }

    /**
     * Sets the value of the cDoubleFloat property.
     * 
     */
    public void setCDoubleFloat(double value) {
        this.cDoubleFloat = value;
    }

    /**
     * Gets the value of the cPackedDecimal property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getCPackedDecimal() {
        return cPackedDecimal;
    }

    /**
     * Sets the value of the cPackedDecimal property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setCPackedDecimal(BigDecimal value) {
        this.cPackedDecimal = value;
    }

    /**
     * Gets the value of the cZonedDecimal property.
     * 
     */
    public long getCZonedDecimal() {
        return cZonedDecimal;
    }

    /**
     * Sets the value of the cZonedDecimal property.
     * 
     */
    public void setCZonedDecimal(long value) {
        this.cZonedDecimal = value;
    }

    /**
     * Gets the value of the cNumericEdited1 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCNumericEdited1() {
        return cNumericEdited1;
    }

    /**
     * Sets the value of the cNumericEdited1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCNumericEdited1(String value) {
        this.cNumericEdited1 = value;
    }

    /**
     * Gets the value of the cNumericEdited2 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCNumericEdited2() {
        return cNumericEdited2;
    }

    /**
     * Sets the value of the cNumericEdited2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCNumericEdited2(String value) {
        this.cNumericEdited2 = value;
    }

    /**
     * Gets the value of the cNumericEdited3 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCNumericEdited3() {
        return cNumericEdited3;
    }

    /**
     * Sets the value of the cNumericEdited3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCNumericEdited3(String value) {
        this.cNumericEdited3 = value;
    }

    /**
     * Gets the value of the cNumericEdited4 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCNumericEdited4() {
        return cNumericEdited4;
    }

    /**
     * Sets the value of the cNumericEdited4 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCNumericEdited4(String value) {
        this.cNumericEdited4 = value;
    }

    /**
     * Gets the value of the cIndex property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public byte[] getCIndex() {
        return cIndex;
    }

    /**
     * Sets the value of the cIndex property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCIndex(byte[] value) {
        this.cIndex = ((byte[]) value);
    }

    /**
     * Gets the value of the cPointer property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public byte[] getCPointer() {
        return cPointer;
    }

    /**
     * Sets the value of the cPointer property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCPointer(byte[] value) {
        this.cPointer = ((byte[]) value);
    }

    /**
     * Gets the value of the cProcPointer property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public byte[] getCProcPointer() {
        return cProcPointer;
    }

    /**
     * Sets the value of the cProcPointer property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCProcPointer(byte[] value) {
        this.cProcPointer = ((byte[]) value);
    }

    /**
     * Gets the value of the cFuncPointer property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public byte[] getCFuncPointer() {
        return cFuncPointer;
    }

    /**
     * Sets the value of the cFuncPointer property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCFuncPointer(byte[] value) {
        this.cFuncPointer = ((byte[]) value);
    }

    /**
     * Gets the value of the cExternalFloating property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCExternalFloating() {
        return cExternalFloating;
    }

    /**
     * Sets the value of the cExternalFloating property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCExternalFloating(String value) {
        this.cExternalFloating = value;
    }

    /**
     * Gets the value of the cBinary property.
     * 
     */
    public int getCBinary() {
        return cBinary;
    }

    /**
     * Sets the value of the cBinary property.
     * 
     */
    public void setCBinary(int value) {
        this.cBinary = value;
    }

    /**
     * Gets the value of the cNativeBinary property.
     * 
     */
    public int getCNativeBinary() {
        return cNativeBinary;
    }

    /**
     * Sets the value of the cNativeBinary property.
     * 
     */
    public void setCNativeBinary(int value) {
        this.cNativeBinary = value;
    }

}

//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.1-b02-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.04.23 at 05:00:22 PM CEST 
//


package com.legstar.test.coxb.binnatsi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolType;
import com.legstar.coxb.annotation.CobolElement;


/**
 * <p>Java class for WsHalfwordsType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WsHalfwordsType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="WsPs9X4Min">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}short">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsPs9X4Low">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}short">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsPs9X4High">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}short">
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsPs9X4Max">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}short">
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
@XmlType(name = "WsHalfwordsType", propOrder = {
    "wsPs9X4Min",
    "wsPs9X4Low",
    "wsPs9X4High",
    "wsPs9X4Max"
})
public class WsHalfwordsType {

    @XmlElement(name = "WsPs9X4Min")
    @CobolElement(cobolName = "WS-PS9X4-MIN", type = CobolType.NATIVE_BINARY_ITEM, byteLength = 2, isJustifiedRight = false, isSigned = true, isSignLeading = false, isSignSeparate = false, totalDigits = 4)
    protected short wsPs9X4Min;
    @XmlElement(name = "WsPs9X4Low")
    @CobolElement(cobolName = "WS-PS9X4-LOW", type = CobolType.NATIVE_BINARY_ITEM, byteLength = 2, isJustifiedRight = false, isSigned = true, isSignLeading = false, isSignSeparate = false, totalDigits = 4)
    protected short wsPs9X4Low;
    @XmlElement(name = "WsPs9X4High")
    @CobolElement(cobolName = "WS-PS9X4-HIGH", type = CobolType.NATIVE_BINARY_ITEM, byteLength = 2, isJustifiedRight = false, isSigned = true, isSignLeading = false, isSignSeparate = false, totalDigits = 4)
    protected short wsPs9X4High;
    @XmlElement(name = "WsPs9X4Max")
    @CobolElement(cobolName = "WS-PS9X4-MAX", type = CobolType.NATIVE_BINARY_ITEM, byteLength = 2, isJustifiedRight = false, isSigned = true, isSignLeading = false, isSignSeparate = false, totalDigits = 4)
    protected short wsPs9X4Max;

    /**
     * Gets the value of the wsPs9X4Min property.
     * 
     */
    public short getWsPs9X4Min() {
        return wsPs9X4Min;
    }

    /**
     * Sets the value of the wsPs9X4Min property.
     * 
     */
    public void setWsPs9X4Min(short value) {
        this.wsPs9X4Min = value;
    }

    public boolean isSetWsPs9X4Min() {
        return true;
    }

    /**
     * Gets the value of the wsPs9X4Low property.
     * 
     */
    public short getWsPs9X4Low() {
        return wsPs9X4Low;
    }

    /**
     * Sets the value of the wsPs9X4Low property.
     * 
     */
    public void setWsPs9X4Low(short value) {
        this.wsPs9X4Low = value;
    }

    public boolean isSetWsPs9X4Low() {
        return true;
    }

    /**
     * Gets the value of the wsPs9X4High property.
     * 
     */
    public short getWsPs9X4High() {
        return wsPs9X4High;
    }

    /**
     * Sets the value of the wsPs9X4High property.
     * 
     */
    public void setWsPs9X4High(short value) {
        this.wsPs9X4High = value;
    }

    public boolean isSetWsPs9X4High() {
        return true;
    }

    /**
     * Gets the value of the wsPs9X4Max property.
     * 
     */
    public short getWsPs9X4Max() {
        return wsPs9X4Max;
    }

    /**
     * Sets the value of the wsPs9X4Max property.
     * 
     */
    public void setWsPs9X4Max(short value) {
        this.wsPs9X4Max = value;
    }

    public boolean isSetWsPs9X4Max() {
        return true;
    }

}
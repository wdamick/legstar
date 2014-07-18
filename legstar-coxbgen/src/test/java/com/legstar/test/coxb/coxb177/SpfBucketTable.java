package com.legstar.test.coxb.coxb177;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;

/**
 * <p>
 * Java class for SpfBucketTable complex type.
 * 
 * <p>
 * The following schema fragment specifies the expected content contained within
 * this class.
 * 
 * <pre>
 * &lt;complexType name="SpfBucketTable">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="filler6">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="4"/>
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
@XmlType(name = "SpfBucketTable", propOrder = { "filler6" })
public class SpfBucketTable implements Serializable {

    private final static long serialVersionUID = 1L;
    @XmlElement(required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 10, picture = "X(4)", srceLine = 6)
    protected String filler6;

    /**
     * Gets the value of the filler6 property.
     * 
     * @return possible object is {@link String }
     * 
     */
    public String getFiller6() {
        return filler6;
    }

    /**
     * Sets the value of the filler6 property.
     * 
     * @param value allowed object is {@link String }
     * 
     */
    public void setFiller6(String value) {
        this.filler6 = value;
    }

    public boolean isSetFiller6() {
        return (this.filler6 != null);
    }

}

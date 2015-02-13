
package com.legstar.test.coxb.arrayssm;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for WsTs complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WsTs">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="Filler21">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="2"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsTsA">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *               &lt;totalDigits value="1"/>
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
@XmlType(name = "WsTs", propOrder = {
    "filler21",
    "wsTsA"
})
public class WsTs
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "Filler21", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(2)", value = "TS", srceLine = 21)
    protected String filler21 = "TS";
    @XmlElement(name = "WsTsA")
    @CobolElement(cobolName = "WS-TS-A", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 1, picture = "9(1)", srceLine = 22)
    protected int wsTsA;

    /**
     * Gets the value of the filler21 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller21() {
        return filler21;
    }

    /**
     * Sets the value of the filler21 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller21(String value) {
        this.filler21 = value;
    }

    public boolean isSetFiller21() {
        return (this.filler21 != null);
    }

    /**
     * Gets the value of the wsTsA property.
     * 
     */
    public int getWsTsA() {
        return wsTsA;
    }

    /**
     * Sets the value of the wsTsA property.
     * 
     */
    public void setWsTsA(int value) {
        this.wsTsA = value;
    }

    public boolean isSetWsTsA() {
        return true;
    }

}

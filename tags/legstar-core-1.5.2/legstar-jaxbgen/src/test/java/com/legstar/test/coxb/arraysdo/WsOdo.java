
package com.legstar.test.coxb.arraysdo;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for WsOdo complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WsOdo">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="Filler21">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="3"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="WsOdoA">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *               &lt;totalDigits value="2"/>
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
@XmlType(name = "WsOdo", propOrder = {
    "filler21",
    "wsOdoA"
})
public class WsOdo
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "Filler21", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(3)", value = "ODO", srceLine = 21)
    protected String filler21 = "ODO";
    @XmlElement(name = "WsOdoA")
    @CobolElement(cobolName = "WS-ODO-A", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 2, picture = "9(2)", srceLine = 22)
    protected int wsOdoA;

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
     * Gets the value of the wsOdoA property.
     * 
     */
    public int getWsOdoA() {
        return wsOdoA;
    }

    /**
     * Sets the value of the wsOdoA property.
     * 
     */
    public void setWsOdoA(int value) {
        this.wsOdoA = value;
    }

    public boolean isSetWsOdoA() {
        return true;
    }

}

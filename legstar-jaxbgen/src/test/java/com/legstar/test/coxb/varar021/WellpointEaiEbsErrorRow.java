
package com.legstar.test.coxb.varar021;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for WellpointEaiEbsErrorRow complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WellpointEaiEbsErrorRow">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="WeerExceptionUuid">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="32"/>
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
@XmlType(name = "WellpointEaiEbsErrorRow", propOrder = {
    "weerExceptionUuid"
})
public class WellpointEaiEbsErrorRow
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "WeerExceptionUuid", required = true)
    @CobolElement(cobolName = "WEER-EXCEPTION-UUID", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 10, picture = "X(32)", srceLine = 49)
    protected String weerExceptionUuid;

    /**
     * Gets the value of the weerExceptionUuid property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getWeerExceptionUuid() {
        return weerExceptionUuid;
    }

    /**
     * Sets the value of the weerExceptionUuid property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setWeerExceptionUuid(String value) {
        this.weerExceptionUuid = value;
    }

    public boolean isSetWeerExceptionUuid() {
        return (this.weerExceptionUuid!= null);
    }

}

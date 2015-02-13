
package com.legstar.test.coxb.varar021;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for SearchGrplst complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="SearchGrplst">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="Payload" type="{http://legstar.com/test/coxb/varar021}Payload"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "SearchGrplst", propOrder = {
    "payload"
})
public class SearchGrplst
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "Payload", required = true)
    @CobolElement(cobolName = "PAYLOAD", type = CobolType.GROUP_ITEM, levelNumber = 3, srceLine = 32)
    protected Payload payload;

    /**
     * Gets the value of the payload property.
     * 
     * @return
     *     possible object is
     *     {@link Payload }
     *     
     */
    public Payload getPayload() {
        return payload;
    }

    /**
     * Sets the value of the payload property.
     * 
     * @param value
     *     allowed object is
     *     {@link Payload }
     *     
     */
    public void setPayload(Payload value) {
        this.payload = value;
    }

    public boolean isSetPayload() {
        return (this.payload!= null);
    }

}

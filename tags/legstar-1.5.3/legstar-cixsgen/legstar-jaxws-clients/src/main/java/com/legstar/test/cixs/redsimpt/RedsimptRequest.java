
package com.legstar.test.cixs.redsimpt;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.test.coxb.redsimpt.Dfhcommarea;


/**
 * <p>Java class for RedsimptRequest complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="RedsimptRequest">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{http://legstar.com/test/coxb/redsimpt}Dfhcommarea"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "RedsimptRequest", propOrder = {
    "dfhcommarea"
})
public class RedsimptRequest {

    @XmlElement(name = "Dfhcommarea", namespace = "http://legstar.com/test/coxb/redsimpt", required = true)
    protected Dfhcommarea dfhcommarea;

    /**
     * Gets the value of the dfhcommarea property.
     * 
     * @return
     *     possible object is
     *     {@link Dfhcommarea }
     *     
     */
    public Dfhcommarea getDfhcommarea() {
        return dfhcommarea;
    }

    /**
     * Sets the value of the dfhcommarea property.
     * 
     * @param value
     *     allowed object is
     *     {@link Dfhcommarea }
     *     
     */
    public void setDfhcommarea(Dfhcommarea value) {
        this.dfhcommarea = value;
    }

}

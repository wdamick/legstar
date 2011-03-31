
package com.legstar.test.cixs.listssdo;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.test.coxb.listssdo.Dfhcommarea;


/**
 * <p>Java class for ListssdoRequest complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ListssdoRequest">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{http://legstar.com/test/coxb/listssdo}Dfhcommarea"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ListssdoRequest", propOrder = {
    "dfhcommarea"
})
public class ListssdoRequest {

    @XmlElement(name = "Dfhcommarea", namespace = "http://legstar.com/test/coxb/listssdo", required = true)
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

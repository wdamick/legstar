
package com.legstar.test.cixs.varar021;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.test.coxb.varar021.SearchGrplst;


/**
 * <p>Java class for Varar021Request complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Varar021Request">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{http://legstar.com/test/coxb/varar021}SearchGrplst"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Varar021Request", propOrder = {
    "searchGrplst"
})
public class Varar021Request {

    @XmlElement(name = "SearchGrplst", namespace = "http://legstar.com/test/coxb/varar021", required = true)
    protected SearchGrplst searchGrplst;

    /**
     * Gets the value of the searchGrplst property.
     * 
     * @return
     *     possible object is
     *     {@link SearchGrplst }
     *     
     */
    public SearchGrplst getSearchGrplst() {
        return searchGrplst;
    }

    /**
     * Sets the value of the searchGrplst property.
     * 
     * @param value
     *     allowed object is
     *     {@link SearchGrplst }
     *     
     */
    public void setSearchGrplst(SearchGrplst value) {
        this.searchGrplst = value;
    }

}

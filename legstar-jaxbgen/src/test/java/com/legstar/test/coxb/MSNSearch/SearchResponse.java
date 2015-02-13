
package com.legstar.test.coxb.MSNSearch;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for anonymous complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType>
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="Response" type="{http://schemas.microsoft.com/MSNSearch/2005/09/fex}SearchResponse"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "", propOrder = {
    "response"
})
@XmlRootElement(name = "SearchResponse")
public class SearchResponse
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "Response", required = true)
    @CobolElement(cobolName = "Response", type = CobolType.GROUP_ITEM, levelNumber = 3)
    protected SearchResponseType response;

    /**
     * Gets the value of the response property.
     * 
     * @return
     *     possible object is
     *     {@link SearchResponseType }
     *     
     */
    public SearchResponseType getResponse() {
        return response;
    }

    /**
     * Sets the value of the response property.
     * 
     * @param value
     *     allowed object is
     *     {@link SearchResponseType }
     *     
     */
    public void setResponse(SearchResponseType value) {
        this.response = value;
    }

    public boolean isSetResponse() {
        return (this.response!= null);
    }

}

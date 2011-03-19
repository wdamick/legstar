package com.legstar.test.cixs.binpkdus;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import com.legstar.test.coxb.binpkdus.Dfhcommarea;

/**
 * LegStar/Jaxws Operation Request Wrapper.
 * A wrapper class for the Request.
 * 
 * This class was generated by LegStar Mainframe Web Service adapter generator.
 */

@XmlRootElement(name = "BinpkdusRequest")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "BinpkdusRequest",
         namespace = "http://cixs.test.legstar.com/alltypes",
         propOrder = {
    "request"
})
public class BinpkdusRequest {
    
    /** Inner JAXB-bound object. */
    @XmlElement(name = "Dfhcommarea",
                namespace = "http://legstar.com/test/coxb/binpkdus",
                required = true)
    private Dfhcommarea request;

    /**
    * Get the inner JAXB-bound object.
    * 
    * @return JAXB-bound object
    */
    public Dfhcommarea getRequest() {
        return request;
    }

  /**
   * Set the inner JAXB-bound object.
   * 
   * @param value JAXB-bound object
   */
    public void setRequest(
            final Dfhcommarea value) {
        request = value;
    }

}

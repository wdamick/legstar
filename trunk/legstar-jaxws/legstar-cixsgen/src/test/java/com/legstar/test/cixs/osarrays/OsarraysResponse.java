package com.legstar.test.cixs.osarrays;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import com.legstar.test.coxb.osarrays.Dfhcommarea;

/**
 * LegStar/Jaxws Operation Response Wrapper.
 * A wrapper class for the Response.
 * 
 * This class was generated by LegStar Mainframe Web Service adapter generator.
 */

@XmlRootElement(name = "OsarraysResponse")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "OsarraysResponse",
         namespace = "http://cixs.test.legstar.com/alltypes",
         propOrder = {
    "response"
})
public class OsarraysResponse {
    
    /** Inner JAXB-bound object. */
    @XmlElement(name = "Dfhcommarea",
                namespace = "http://legstar.com/test/coxb/osarrays",
                required = true)
    private Dfhcommarea response;

    /**
    * Get the inner JAXB-bound object.
    * 
    * @return JAXB-bound object
    */
    public Dfhcommarea getResponse() {
        return response;
    }

  /**
   * Set the inner JAXB-bound object.
   * 
   * @param value JAXB-bound object
   */
    public void setResponse(
            final Dfhcommarea value) {
        response = value;
    }

}

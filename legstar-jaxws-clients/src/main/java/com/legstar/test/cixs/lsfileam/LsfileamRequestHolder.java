
package com.legstar.test.cixs.lsfileam;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.test.coxb.lsfileac.QueryData;
import com.legstar.test.coxb.lsfileac.QueryLimit;


/**
 * <p>Java class for LsfileamRequestHolder complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsfileamRequestHolder">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{http://legstar.com/test/coxb/lsfileac}QueryData"/>
 *         &lt;element ref="{http://legstar.com/test/coxb/lsfileac}QueryLimit"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsfileamRequestHolder", propOrder = {
    "queryData",
    "queryLimit"
})
public class LsfileamRequestHolder {

    @XmlElement(name = "QueryData", namespace = "http://legstar.com/test/coxb/lsfileac", required = true)
    protected QueryData queryData;
    @XmlElement(name = "QueryLimit", namespace = "http://legstar.com/test/coxb/lsfileac", required = true)
    protected QueryLimit queryLimit;

    /**
     * Gets the value of the queryData property.
     * 
     * @return
     *     possible object is
     *     {@link QueryData }
     *     
     */
    public QueryData getQueryData() {
        return queryData;
    }

    /**
     * Sets the value of the queryData property.
     * 
     * @param value
     *     allowed object is
     *     {@link QueryData }
     *     
     */
    public void setQueryData(QueryData value) {
        this.queryData = value;
    }

    /**
     * Gets the value of the queryLimit property.
     * 
     * @return
     *     possible object is
     *     {@link QueryLimit }
     *     
     */
    public QueryLimit getQueryLimit() {
        return queryLimit;
    }

    /**
     * Sets the value of the queryLimit property.
     * 
     * @param value
     *     allowed object is
     *     {@link QueryLimit }
     *     
     */
    public void setQueryLimit(QueryLimit value) {
        this.queryLimit = value;
    }

}

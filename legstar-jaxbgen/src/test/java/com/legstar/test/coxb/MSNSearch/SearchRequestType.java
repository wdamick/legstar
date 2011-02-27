
package com.legstar.test.coxb.MSNSearch;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlList;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for SearchRequest complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="SearchRequest">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="AppID" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="Query" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="CultureInfo" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="SafeSearch" type="{http://schemas.microsoft.com/MSNSearch/2005/09/fex}SafeSearchOptions"/>
 *         &lt;element name="Flags" type="{http://schemas.microsoft.com/MSNSearch/2005/09/fex}SearchFlags"/>
 *         &lt;element name="Location" type="{http://schemas.microsoft.com/MSNSearch/2005/09/fex}Location" minOccurs="0"/>
 *         &lt;element name="Requests" type="{http://schemas.microsoft.com/MSNSearch/2005/09/fex}ArrayOfSourceRequestRequests"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "SearchRequest", propOrder = {
    "appID",
    "query",
    "cultureInfo",
    "safeSearch",
    "flags",
    "location",
    "requests"
})
public class SearchRequestType
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "AppID", required = true)
    @CobolElement(cobolName = "AppID", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(40)", usage = "DISPLAY")
    protected String appID;
    @XmlElement(name = "Query", required = true)
    @CobolElement(cobolName = "Query", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(128)", usage = "DISPLAY")
    protected String query;
    @XmlElement(name = "CultureInfo", required = true)
    @CobolElement(cobolName = "CultureInfo", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(32)", usage = "DISPLAY")
    protected String cultureInfo;
    @XmlElement(name = "SafeSearch", required = true, defaultValue = "Moderate")
    @CobolElement(cobolName = "SafeSearch", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(32)", usage = "DISPLAY")
    protected SafeSearchOptionsType safeSearch;
    @XmlList
    @XmlElement(name = "Flags", required = true, defaultValue = "None")
    @CobolElement(cobolName = "Flags", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, minOccurs = 1, maxOccurs = 10, picture = "X(32)", usage = "DISPLAY")
    protected List<String> flags;
    @XmlElement(name = "Location")
    @CobolElement(cobolName = "Location", type = CobolType.GROUP_ITEM, levelNumber = 5)
    protected LocationType location;
    @XmlElement(name = "Requests", required = true)
    @CobolElement(cobolName = "Requests", type = CobolType.GROUP_ITEM, levelNumber = 5)
    protected ArrayOfSourceRequestRequestsType requests;

    /**
     * Gets the value of the appID property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAppID() {
        return appID;
    }

    /**
     * Sets the value of the appID property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAppID(String value) {
        this.appID = value;
    }

    public boolean isSetAppID() {
        return (this.appID!= null);
    }

    /**
     * Gets the value of the query property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getQuery() {
        return query;
    }

    /**
     * Sets the value of the query property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setQuery(String value) {
        this.query = value;
    }

    public boolean isSetQuery() {
        return (this.query!= null);
    }

    /**
     * Gets the value of the cultureInfo property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCultureInfo() {
        return cultureInfo;
    }

    /**
     * Sets the value of the cultureInfo property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCultureInfo(String value) {
        this.cultureInfo = value;
    }

    public boolean isSetCultureInfo() {
        return (this.cultureInfo!= null);
    }

    /**
     * Gets the value of the safeSearch property.
     * 
     * @return
     *     possible object is
     *     {@link SafeSearchOptionsType }
     *     
     */
    public SafeSearchOptionsType getSafeSearch() {
        return safeSearch;
    }

    /**
     * Sets the value of the safeSearch property.
     * 
     * @param value
     *     allowed object is
     *     {@link SafeSearchOptionsType }
     *     
     */
    public void setSafeSearch(SafeSearchOptionsType value) {
        this.safeSearch = value;
    }

    public boolean isSetSafeSearch() {
        return (this.safeSearch!= null);
    }

    /**
     * Gets the value of the flags property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the flags property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getFlags().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link String }
     * 
     * 
     */
    public List<String> getFlags() {
        if (flags == null) {
            flags = new ArrayList<String>();
        }
        return this.flags;
    }

    public boolean isSetFlags() {
        return ((this.flags!= null)&&(!this.flags.isEmpty()));
    }

    public void unsetFlags() {
        this.flags = null;
    }

    /**
     * Gets the value of the location property.
     * 
     * @return
     *     possible object is
     *     {@link LocationType }
     *     
     */
    public LocationType getLocation() {
        return location;
    }

    /**
     * Sets the value of the location property.
     * 
     * @param value
     *     allowed object is
     *     {@link LocationType }
     *     
     */
    public void setLocation(LocationType value) {
        this.location = value;
    }

    public boolean isSetLocation() {
        return (this.location!= null);
    }

    /**
     * Gets the value of the requests property.
     * 
     * @return
     *     possible object is
     *     {@link ArrayOfSourceRequestRequestsType }
     *     
     */
    public ArrayOfSourceRequestRequestsType getRequests() {
        return requests;
    }

    /**
     * Sets the value of the requests property.
     * 
     * @param value
     *     allowed object is
     *     {@link ArrayOfSourceRequestRequestsType }
     *     
     */
    public void setRequests(ArrayOfSourceRequestRequestsType value) {
        this.requests = value;
    }

    public boolean isSetRequests() {
        return (this.requests!= null);
    }

}

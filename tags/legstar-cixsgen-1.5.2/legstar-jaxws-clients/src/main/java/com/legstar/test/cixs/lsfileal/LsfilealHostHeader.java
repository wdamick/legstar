
package com.legstar.test.cixs.lsfileal;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsfilealHostHeader complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsfilealHostHeader">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="hostUserID" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="hostPassword" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="hostEndPoint" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="hostCharset" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="hostTraceMode" type="{http://www.w3.org/2001/XMLSchema}boolean"/>
 *         &lt;element name="hostRequestID" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsfilealHostHeader", propOrder = {
    "hostUserID",
    "hostPassword",
    "hostEndPoint",
    "hostCharset",
    "hostTraceMode",
    "hostRequestID"
})
public class LsfilealHostHeader {

    protected String hostUserID;
    protected String hostPassword;
    protected String hostEndPoint;
    protected String hostCharset;
    protected boolean hostTraceMode;
    protected String hostRequestID;

    /**
     * Gets the value of the hostUserID property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getHostUserID() {
        return hostUserID;
    }

    /**
     * Sets the value of the hostUserID property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setHostUserID(String value) {
        this.hostUserID = value;
    }

    /**
     * Gets the value of the hostPassword property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getHostPassword() {
        return hostPassword;
    }

    /**
     * Sets the value of the hostPassword property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setHostPassword(String value) {
        this.hostPassword = value;
    }

    /**
     * Gets the value of the hostEndPoint property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getHostEndPoint() {
        return hostEndPoint;
    }

    /**
     * Sets the value of the hostEndPoint property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setHostEndPoint(String value) {
        this.hostEndPoint = value;
    }

    /**
     * Gets the value of the hostCharset property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getHostCharset() {
        return hostCharset;
    }

    /**
     * Sets the value of the hostCharset property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setHostCharset(String value) {
        this.hostCharset = value;
    }

    /**
     * Gets the value of the hostTraceMode property.
     * 
     */
    public boolean isHostTraceMode() {
        return hostTraceMode;
    }

    /**
     * Sets the value of the hostTraceMode property.
     * 
     */
    public void setHostTraceMode(boolean value) {
        this.hostTraceMode = value;
    }

    /**
     * Gets the value of the hostRequestID property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getHostRequestID() {
        return hostRequestID;
    }

    /**
     * Sets the value of the hostRequestID property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setHostRequestID(String value) {
        this.hostRequestID = value;
    }

}

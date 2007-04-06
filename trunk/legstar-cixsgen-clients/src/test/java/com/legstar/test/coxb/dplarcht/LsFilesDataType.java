
package com.legstar.test.coxb.dplarcht;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for LsFilesDataType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsFilesDataType">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsFileName" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="LsFileDsname" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="LsFileEnablestatus" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsFilesDataType", propOrder = {
    "lsFileName",
    "lsFileDsname",
    "lsFileEnablestatus"
})
public class LsFilesDataType {

    @XmlElement(name = "LsFileName", required = true)
    protected String lsFileName;
    @XmlElement(name = "LsFileDsname", required = true)
    protected String lsFileDsname;
    @XmlElement(name = "LsFileEnablestatus", required = true)
    protected String lsFileEnablestatus;

    /**
     * Gets the value of the lsFileName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLsFileName() {
        return lsFileName;
    }

    /**
     * Sets the value of the lsFileName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLsFileName(String value) {
        this.lsFileName = value;
    }

    /**
     * Gets the value of the lsFileDsname property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLsFileDsname() {
        return lsFileDsname;
    }

    /**
     * Sets the value of the lsFileDsname property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLsFileDsname(String value) {
        this.lsFileDsname = value;
    }

    /**
     * Gets the value of the lsFileEnablestatus property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLsFileEnablestatus() {
        return lsFileEnablestatus;
    }

    /**
     * Sets the value of the lsFileEnablestatus property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLsFileEnablestatus(String value) {
        this.lsFileEnablestatus = value;
    }

}


package com.legstar.test.coxb.lsfileal;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for ReplySuccessHeader complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ReplySuccessHeader">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="SearchDuration">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="TotalItemsRead">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *               &lt;totalDigits value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler60">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="123"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ReplySuccessHeader", propOrder = {
    "searchDuration",
    "totalItemsRead",
    "filler60"
})
public class ReplySuccessHeader
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "SearchDuration", required = true)
    @CobolElement(cobolName = "SEARCH-DURATION", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 10, picture = "X(8)", srceLine = 58)
    protected String searchDuration;
    @XmlElement(name = "TotalItemsRead")
    @CobolElement(cobolName = "TOTAL-ITEMS-READ", type = CobolType.PACKED_DECIMAL_ITEM, levelNumber = 10, isSigned = false, totalDigits = 8, picture = "9(8)", usage = "PACKED-DECIMAL", srceLine = 59)
    protected long totalItemsRead;
    @XmlElement(name = "Filler60", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 10, picture = "X(123)", srceLine = 60)
    protected String filler60;

    /**
     * Gets the value of the searchDuration property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSearchDuration() {
        return searchDuration;
    }

    /**
     * Sets the value of the searchDuration property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSearchDuration(String value) {
        this.searchDuration = value;
    }

    public boolean isSetSearchDuration() {
        return (this.searchDuration!= null);
    }

    /**
     * Gets the value of the totalItemsRead property.
     * 
     */
    public long getTotalItemsRead() {
        return totalItemsRead;
    }

    /**
     * Sets the value of the totalItemsRead property.
     * 
     */
    public void setTotalItemsRead(long value) {
        this.totalItemsRead = value;
    }

    public boolean isSetTotalItemsRead() {
        return true;
    }

    /**
     * Gets the value of the filler60 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller60() {
        return filler60;
    }

    /**
     * Sets the value of the filler60 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller60(String value) {
        this.filler60 = value;
    }

    public boolean isSetFiller60() {
        return (this.filler60 != null);
    }

}

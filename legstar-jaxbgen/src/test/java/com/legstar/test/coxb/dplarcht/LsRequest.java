
package com.legstar.test.coxb.dplarcht;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for LsRequest complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LsRequest">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="LsRequestType">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *               &lt;totalDigits value="4"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;choice>
 *           &lt;element name="LsAllItems">
 *             &lt;simpleType>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *                 &lt;maxLength value="4"/>
 *               &lt;/restriction>
 *             &lt;/simpleType>
 *           &lt;/element>
 *           &lt;element name="LsMaxItems">
 *             &lt;simpleType>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *                 &lt;totalDigits value="4"/>
 *               &lt;/restriction>
 *             &lt;/simpleType>
 *           &lt;/element>
 *         &lt;/choice>
 *         &lt;element name="LsSearchCriteria" type="{http://legstar.com/test/coxb/dplarcht}LsSearchCriteria"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsRequest", propOrder = {
    "lsRequestType",
    "lsAllItems",
    "lsMaxItems",
    "lsSearchCriteria"
})
public class LsRequest
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "LsRequestType")
    @CobolElement(cobolName = "LS-REQUEST-TYPE", type = CobolType.BINARY_ITEM, levelNumber = 10, isSigned = false, totalDigits = 4, picture = "9(4)", usage = "BINARY", isCustomVariable = true, srceLine = 82)
    protected int lsRequestType;
    @XmlElement(name = "LsAllItems")
    @CobolElement(cobolName = "LS-ALL-ITEMS", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 10, isRedefined = true, picture = "X(4)", srceLine = 86)
    protected String lsAllItems;
    @XmlElement(name = "LsMaxItems")
    @CobolElement(cobolName = "LS-MAX-ITEMS", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 10, isSigned = false, totalDigits = 4, redefines = "LS-ALL-ITEMS", picture = "9(4)", srceLine = 87)
    protected Integer lsMaxItems;
    @XmlElement(name = "LsSearchCriteria", required = true)
    @CobolElement(cobolName = "LS-SEARCH-CRITERIA", type = CobolType.GROUP_ITEM, levelNumber = 10, srceLine = 89)
    protected LsSearchCriteria lsSearchCriteria;

    /**
     * Gets the value of the lsRequestType property.
     * 
     */
    public int getLsRequestType() {
        return lsRequestType;
    }

    /**
     * Sets the value of the lsRequestType property.
     * 
     */
    public void setLsRequestType(int value) {
        this.lsRequestType = value;
    }

    public boolean isSetLsRequestType() {
        return true;
    }

    /**
     * Gets the value of the lsAllItems property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLsAllItems() {
        return lsAllItems;
    }

    /**
     * Sets the value of the lsAllItems property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLsAllItems(String value) {
        this.lsAllItems = value;
    }

    public boolean isSetLsAllItems() {
        return (this.lsAllItems!= null);
    }

    /**
     * Gets the value of the lsMaxItems property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getLsMaxItems() {
        return lsMaxItems;
    }

    /**
     * Sets the value of the lsMaxItems property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setLsMaxItems(Integer value) {
        this.lsMaxItems = value;
    }

    public boolean isSetLsMaxItems() {
        return (this.lsMaxItems!= null);
    }

    /**
     * Gets the value of the lsSearchCriteria property.
     * 
     * @return
     *     possible object is
     *     {@link LsSearchCriteria }
     *     
     */
    public LsSearchCriteria getLsSearchCriteria() {
        return lsSearchCriteria;
    }

    /**
     * Sets the value of the lsSearchCriteria property.
     * 
     * @param value
     *     allowed object is
     *     {@link LsSearchCriteria }
     *     
     */
    public void setLsSearchCriteria(LsSearchCriteria value) {
        this.lsSearchCriteria = value;
    }

    public boolean isSetLsSearchCriteria() {
        return (this.lsSearchCriteria!= null);
    }

}

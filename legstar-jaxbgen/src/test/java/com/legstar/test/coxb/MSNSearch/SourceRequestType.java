
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
 * <p>Java class for SourceRequest complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="SourceRequest">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="Source" type="{http://schemas.microsoft.com/MSNSearch/2005/09/fex}SourceType"/>
 *         &lt;element name="Offset" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="Count" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="FileType" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="SortBy" type="{http://schemas.microsoft.com/MSNSearch/2005/09/fex}SortByType" minOccurs="0"/>
 *         &lt;element name="ResultFields" type="{http://schemas.microsoft.com/MSNSearch/2005/09/fex}ResultFieldMask"/>
 *         &lt;element name="SearchTagFilters" type="{http://schemas.microsoft.com/MSNSearch/2005/09/fex}ArrayOfstringSearchTagFilters" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "SourceRequest", propOrder = {
    "source",
    "offset",
    "count",
    "fileType",
    "sortBy",
    "resultFields",
    "searchTagFilters"
})
public class SourceRequestType
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "Source", required = true, defaultValue = "Web")
    @CobolElement(cobolName = "R-Source", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 9, picture = "X(32)", usage = "DISPLAY")
    protected SourceTypeType source;
    @XmlElement(name = "Offset", defaultValue = "0")
    @CobolElement(cobolName = "Offset", type = CobolType.BINARY_ITEM, levelNumber = 9, isSigned = true, totalDigits = 9, picture = "9(9)", usage = "COMP-5")
    protected int offset;
    @XmlElement(name = "Count", defaultValue = "10")
    @CobolElement(cobolName = "R-Count", type = CobolType.BINARY_ITEM, levelNumber = 9, isSigned = true, totalDigits = 9, picture = "9(9)", usage = "COMP-5")
    protected int count;
    @XmlElement(name = "FileType")
    @CobolElement(cobolName = "FileType", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 9, picture = "X(32)", usage = "DISPLAY")
    protected String fileType;
    @XmlList
    @XmlElement(name = "SortBy", defaultValue = "Default")
    @CobolElement(cobolName = "SortBy", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 9, minOccurs = 1, maxOccurs = 10, picture = "X(32)", usage = "DISPLAY")
    protected List<String> sortBy;
    @XmlList
    @XmlElement(name = "ResultFields", required = true, defaultValue = "Title Description Url")
    @CobolElement(cobolName = "ResultFields", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 9, minOccurs = 1, maxOccurs = 10, picture = "X(32)", usage = "DISPLAY")
    protected List<String> resultFields;
    @XmlElement(name = "SearchTagFilters")
    @CobolElement(cobolName = "SearchTagFilters", type = CobolType.GROUP_ITEM, levelNumber = 9)
    protected ArrayOfstringSearchTagFiltersType searchTagFilters;

    /**
     * Gets the value of the source property.
     * 
     * @return
     *     possible object is
     *     {@link SourceTypeType }
     *     
     */
    public SourceTypeType getSource() {
        return source;
    }

    /**
     * Sets the value of the source property.
     * 
     * @param value
     *     allowed object is
     *     {@link SourceTypeType }
     *     
     */
    public void setSource(SourceTypeType value) {
        this.source = value;
    }

    public boolean isSetSource() {
        return (this.source!= null);
    }

    /**
     * Gets the value of the offset property.
     * 
     */
    public int getOffset() {
        return offset;
    }

    /**
     * Sets the value of the offset property.
     * 
     */
    public void setOffset(int value) {
        this.offset = value;
    }

    public boolean isSetOffset() {
        return true;
    }

    /**
     * Gets the value of the count property.
     * 
     */
    public int getCount() {
        return count;
    }

    /**
     * Sets the value of the count property.
     * 
     */
    public void setCount(int value) {
        this.count = value;
    }

    public boolean isSetCount() {
        return true;
    }

    /**
     * Gets the value of the fileType property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFileType() {
        return fileType;
    }

    /**
     * Sets the value of the fileType property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFileType(String value) {
        this.fileType = value;
    }

    public boolean isSetFileType() {
        return (this.fileType!= null);
    }

    /**
     * Gets the value of the sortBy property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the sortBy property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getSortBy().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link String }
     * 
     * 
     */
    public List<String> getSortBy() {
        if (sortBy == null) {
            sortBy = new ArrayList<String>();
        }
        return this.sortBy;
    }

    public boolean isSetSortBy() {
        return ((this.sortBy!= null)&&(!this.sortBy.isEmpty()));
    }

    public void unsetSortBy() {
        this.sortBy = null;
    }

    /**
     * Gets the value of the resultFields property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the resultFields property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getResultFields().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link String }
     * 
     * 
     */
    public List<String> getResultFields() {
        if (resultFields == null) {
            resultFields = new ArrayList<String>();
        }
        return this.resultFields;
    }

    public boolean isSetResultFields() {
        return ((this.resultFields!= null)&&(!this.resultFields.isEmpty()));
    }

    public void unsetResultFields() {
        this.resultFields = null;
    }

    /**
     * Gets the value of the searchTagFilters property.
     * 
     * @return
     *     possible object is
     *     {@link ArrayOfstringSearchTagFiltersType }
     *     
     */
    public ArrayOfstringSearchTagFiltersType getSearchTagFilters() {
        return searchTagFilters;
    }

    /**
     * Sets the value of the searchTagFilters property.
     * 
     * @param value
     *     allowed object is
     *     {@link ArrayOfstringSearchTagFiltersType }
     *     
     */
    public void setSearchTagFilters(ArrayOfstringSearchTagFiltersType value) {
        this.searchTagFilters = value;
    }

    public boolean isSetSearchTagFilters() {
        return (this.searchTagFilters!= null);
    }

}

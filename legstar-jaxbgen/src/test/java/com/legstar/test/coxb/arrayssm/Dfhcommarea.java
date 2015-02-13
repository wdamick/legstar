
package com.legstar.test.coxb.arrayssm;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for Dfhcommarea complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Dfhcommarea">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="TableSimple" maxOccurs="2" minOccurs="2">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="3"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="TableComplex" type="{http://legstar.com/test/coxb/arrayssm}TableComplex" maxOccurs="3" minOccurs="3"/>
 *         &lt;element name="TableComplex2" type="{http://legstar.com/test/coxb/arrayssm}TableComplex2"/>
 *         &lt;element name="TableSimpleNumeric" maxOccurs="5" minOccurs="5">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *               &lt;totalDigits value="1"/>
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
@XmlType(name = "Dfhcommarea", propOrder = {
    "tableSimple",
    "tableComplex",
    "tableComplex2",
    "tableSimpleNumeric"
})
public class Dfhcommarea
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "TableSimple", required = true)
    @CobolElement(cobolName = "TABLE-SIMPLE", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, minOccurs = 2, maxOccurs = 2, picture = "X(3)", srceLine = 32)
    protected List<String> tableSimple;
    @XmlElement(name = "TableComplex", required = true)
    @CobolElement(cobolName = "TABLE-COMPLEX", type = CobolType.GROUP_ITEM, levelNumber = 5, minOccurs = 3, maxOccurs = 3, srceLine = 33)
    protected List<TableComplex> tableComplex;
    @XmlElement(name = "TableComplex2", required = true)
    @CobolElement(cobolName = "TABLE-COMPLEX-2", type = CobolType.GROUP_ITEM, levelNumber = 5, srceLine = 35)
    protected TableComplex2 tableComplex2;
    @XmlElement(name = "TableSimpleNumeric", type = Integer.class)
    @CobolElement(cobolName = "TABLE-SIMPLE-NUMERIC", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 1, minOccurs = 5, maxOccurs = 5, picture = "9", srceLine = 37)
    protected List<Integer> tableSimpleNumeric;

    /**
     * Gets the value of the tableSimple property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the tableSimple property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getTableSimple().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link String }
     * 
     * 
     */
    public List<String> getTableSimple() {
        if (tableSimple == null) {
            tableSimple = new ArrayList<String>();
        }
        return this.tableSimple;
    }

    public boolean isSetTableSimple() {
        return ((this.tableSimple!= null)&&(!this.tableSimple.isEmpty()));
    }

    public void unsetTableSimple() {
        this.tableSimple = null;
    }

    /**
     * Gets the value of the tableComplex property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the tableComplex property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getTableComplex().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link TableComplex }
     * 
     * 
     */
    public List<TableComplex> getTableComplex() {
        if (tableComplex == null) {
            tableComplex = new ArrayList<TableComplex>();
        }
        return this.tableComplex;
    }

    public boolean isSetTableComplex() {
        return ((this.tableComplex!= null)&&(!this.tableComplex.isEmpty()));
    }

    public void unsetTableComplex() {
        this.tableComplex = null;
    }

    /**
     * Gets the value of the tableComplex2 property.
     * 
     * @return
     *     possible object is
     *     {@link TableComplex2 }
     *     
     */
    public TableComplex2 getTableComplex2() {
        return tableComplex2;
    }

    /**
     * Sets the value of the tableComplex2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link TableComplex2 }
     *     
     */
    public void setTableComplex2(TableComplex2 value) {
        this.tableComplex2 = value;
    }

    public boolean isSetTableComplex2() {
        return (this.tableComplex2 != null);
    }

    /**
     * Gets the value of the tableSimpleNumeric property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the tableSimpleNumeric property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getTableSimpleNumeric().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Integer }
     * 
     * 
     */
    public List<Integer> getTableSimpleNumeric() {
        if (tableSimpleNumeric == null) {
            tableSimpleNumeric = new ArrayList<Integer>();
        }
        return this.tableSimpleNumeric;
    }

    public boolean isSetTableSimpleNumeric() {
        return ((this.tableSimpleNumeric!= null)&&(!this.tableSimpleNumeric.isEmpty()));
    }

    public void unsetTableSimpleNumeric() {
        this.tableSimpleNumeric = null;
    }

}

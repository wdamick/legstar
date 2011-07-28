
package com.legstar.test.coxb.arrayssm;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


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
 *         &lt;element name="TableSimple" type="{http://www.w3.org/2001/XMLSchema}string" maxOccurs="unbounded"/>
 *         &lt;element name="TableComplex" type="{http://legstar.com/test/coxb/arrayssm}TableComplex" maxOccurs="unbounded"/>
 *         &lt;element name="TableComplex2" type="{http://legstar.com/test/coxb/arrayssm}TableComplex2"/>
 *         &lt;element name="TableSimpleNumeric" type="{http://www.w3.org/2001/XMLSchema}int" maxOccurs="unbounded" minOccurs="0"/>
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
public class Dfhcommarea {

    @XmlElement(name = "TableSimple", required = true)
    protected List<String> tableSimple;
    @XmlElement(name = "TableComplex", required = true)
    protected List<TableComplex> tableComplex;
    @XmlElement(name = "TableComplex2", required = true)
    protected TableComplex2 tableComplex2;
    @XmlElement(name = "TableSimpleNumeric", type = Integer.class)
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

}

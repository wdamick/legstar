
package com.legstar.test.coxb.arraysdo;

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
 *         &lt;element name="TableSize" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="TableOdo" type="{http://www.w3.org/2001/XMLSchema}string" maxOccurs="unbounded"/>
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
    "tableSize",
    "tableOdo"
})
public class Dfhcommarea {

    @XmlElement(name = "TableSize")
    protected int tableSize;
    @XmlElement(name = "TableOdo", required = true)
    protected List<String> tableOdo;

    /**
     * Gets the value of the tableSize property.
     * 
     */
    public int getTableSize() {
        return tableSize;
    }

    /**
     * Sets the value of the tableSize property.
     * 
     */
    public void setTableSize(int value) {
        this.tableSize = value;
    }

    /**
     * Gets the value of the tableOdo property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the tableOdo property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getTableOdo().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link String }
     * 
     * 
     */
    public List<String> getTableOdo() {
        if (tableOdo == null) {
            tableOdo = new ArrayList<String>();
        }
        return this.tableOdo;
    }

}

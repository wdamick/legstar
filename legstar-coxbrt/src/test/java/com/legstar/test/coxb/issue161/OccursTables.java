
package com.legstar.test.coxb.issue161;

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
 * <p>Java class for OccursTables complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="OccursTables">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="table1" type="{http://coxb.test.legstar.com/issue161}Table1" maxOccurs="10"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "OccursTables", propOrder = {
    "table1"
})
public class OccursTables
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(required = true)
    @CobolElement(cobolName = "TABLE1", type = CobolType.GROUP_ITEM, levelNumber = 10, minOccurs = 1, maxOccurs = 10, dependingOn = "TABLE1-CTR", srceLine = 5)
    protected List<Table1> table1;

    /**
     * Gets the value of the table1 property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the table1 property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getTable1().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Table1 }
     * 
     * 
     */
    public List<Table1> getTable1() {
        if (table1 == null) {
            table1 = new ArrayList<Table1>();
        }
        return this.table1;
    }

    public boolean isSetTable1() {
        return ((this.table1 != null)&&(!this.table1 .isEmpty()));
    }

    public void unsetTable1() {
        this.table1 = null;
    }

}

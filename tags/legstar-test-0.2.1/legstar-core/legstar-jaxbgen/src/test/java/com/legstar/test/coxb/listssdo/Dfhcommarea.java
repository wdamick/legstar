
package com.legstar.test.coxb.listssdo;

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
 *         &lt;element name="ListOdo" maxOccurs="100">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="5"/>
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
    "listOdo"
})
public class Dfhcommarea
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "ListOdo", required = true)
    @CobolElement(cobolName = "LIST-ODO", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, minOccurs = 1, maxOccurs = 100, picture = "X(5)", srceLine = 9)
    protected List<String> listOdo;

    /**
     * Gets the value of the listOdo property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the listOdo property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getListOdo().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link String }
     * 
     * 
     */
    public List<String> getListOdo() {
        if (listOdo == null) {
            listOdo = new ArrayList<String>();
        }
        return this.listOdo;
    }

    public boolean isSetListOdo() {
        return ((this.listOdo!= null)&&(!this.listOdo.isEmpty()));
    }

    public void unsetListOdo() {
        this.listOdo = null;
    }

}

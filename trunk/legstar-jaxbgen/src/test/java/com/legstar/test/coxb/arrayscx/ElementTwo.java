
package com.legstar.test.coxb.arrayscx;

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
 * <p>Java class for ElementTwo complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ElementTwo">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="ElementThree" maxOccurs="2" minOccurs="2">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="3"/>
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
@XmlType(name = "ElementTwo", propOrder = {
    "elementThree"
})
public class ElementTwo
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "ElementThree", required = true)
    @CobolElement(cobolName = "ELEMENT-THREE", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 20, minOccurs = 2, maxOccurs = 2, picture = "X(3)", srceLine = 32)
    protected List<String> elementThree;

    /**
     * Gets the value of the elementThree property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the elementThree property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getElementThree().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link String }
     * 
     * 
     */
    public List<String> getElementThree() {
        if (elementThree == null) {
            elementThree = new ArrayList<String>();
        }
        return this.elementThree;
    }

    public boolean isSetElementThree() {
        return ((this.elementThree!= null)&&(!this.elementThree.isEmpty()));
    }

    public void unsetElementThree() {
        this.elementThree = null;
    }

}

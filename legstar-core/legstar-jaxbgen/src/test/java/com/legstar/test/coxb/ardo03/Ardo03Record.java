
package com.legstar.test.coxb.ardo03;

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
 * <p>Java class for Ardo03Record complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Ardo03Record">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="OdoCounter">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *               &lt;totalDigits value="5"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="OdoArray" type="{http://legstar.com/test/coxb/ardo03}OdoArray" maxOccurs="5" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Ardo03Record", propOrder = {
    "odoCounter",
    "odoArray"
})
public class Ardo03Record
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "OdoCounter")
    @CobolElement(cobolName = "ODO-COUNTER", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 5, isODOObject = true, picture = "9(5)", srceLine = 2)
    protected long odoCounter;
    @XmlElement(name = "OdoArray")
    @CobolElement(cobolName = "ODO-ARRAY", type = CobolType.GROUP_ITEM, levelNumber = 5, minOccurs = -1, maxOccurs = 5, dependingOn = "ODO-COUNTER", srceLine = 3)
    protected List<OdoArray> odoArray;

    /**
     * Gets the value of the odoCounter property.
     * 
     */
    public long getOdoCounter() {
        return odoCounter;
    }

    /**
     * Sets the value of the odoCounter property.
     * 
     */
    public void setOdoCounter(long value) {
        this.odoCounter = value;
    }

    public boolean isSetOdoCounter() {
        return true;
    }

    /**
     * Gets the value of the odoArray property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the odoArray property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getOdoArray().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link OdoArray }
     * 
     * 
     */
    public List<OdoArray> getOdoArray() {
        if (odoArray == null) {
            odoArray = new ArrayList<OdoArray>();
        }
        return this.odoArray;
    }

    public boolean isSetOdoArray() {
        return ((this.odoArray!= null)&&(!this.odoArray.isEmpty()));
    }

    public void unsetOdoArray() {
        this.odoArray = null;
    }

}

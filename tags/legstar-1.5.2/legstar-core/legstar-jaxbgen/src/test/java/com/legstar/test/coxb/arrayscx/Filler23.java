
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
 * <p>Java class for Filler23 complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Filler23">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="WsItemA" maxOccurs="3" minOccurs="3">
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
@XmlType(name = "Filler23", propOrder = {
    "wsItemA"
})
public class Filler23
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "WsItemA", type = Integer.class)
    @CobolElement(cobolName = "WS-ITEM-A", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 5, isSigned = false, totalDigits = 1, minOccurs = 3, maxOccurs = 3, picture = "9", srceLine = 24)
    protected List<Integer> wsItemA;

    /**
     * Gets the value of the wsItemA property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the wsItemA property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getWsItemA().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Integer }
     * 
     * 
     */
    public List<Integer> getWsItemA() {
        if (wsItemA == null) {
            wsItemA = new ArrayList<Integer>();
        }
        return this.wsItemA;
    }

    public boolean isSetWsItemA() {
        return ((this.wsItemA!= null)&&(!this.wsItemA.isEmpty()));
    }

    public void unsetWsItemA() {
        this.wsItemA = null;
    }

}

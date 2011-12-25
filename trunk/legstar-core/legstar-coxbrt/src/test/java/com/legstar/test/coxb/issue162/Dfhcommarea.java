
package com.legstar.test.coxb.issue162;

import java.io.Serializable;
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
 *         &lt;element name="confprty" type="{http://coxb.test.legstar.com/issue162}Confprty"/>
 *         &lt;element name="alteprty" type="{http://coxb.test.legstar.com/issue162}Alteprty"/>
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
    "confprty",
    "alteprty"
})
public class Dfhcommarea
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(required = true)
    @CobolElement(cobolName = "CONFPRTY", type = CobolType.GROUP_ITEM, levelNumber = 10, srceLine = 2)
    protected Confprty confprty;
    @XmlElement(required = true)
    @CobolElement(cobolName = "ALTEPRTY", type = CobolType.GROUP_ITEM, levelNumber = 10, srceLine = 9)
    protected Alteprty alteprty;

    /**
     * Gets the value of the confprty property.
     * 
     * @return
     *     possible object is
     *     {@link Confprty }
     *     
     */
    public Confprty getConfprty() {
        return confprty;
    }

    /**
     * Sets the value of the confprty property.
     * 
     * @param value
     *     allowed object is
     *     {@link Confprty }
     *     
     */
    public void setConfprty(Confprty value) {
        this.confprty = value;
    }

    public boolean isSetConfprty() {
        return (this.confprty!= null);
    }

    /**
     * Gets the value of the alteprty property.
     * 
     * @return
     *     possible object is
     *     {@link Alteprty }
     *     
     */
    public Alteprty getAlteprty() {
        return alteprty;
    }

    /**
     * Sets the value of the alteprty property.
     * 
     * @param value
     *     allowed object is
     *     {@link Alteprty }
     *     
     */
    public void setAlteprty(Alteprty value) {
        this.alteprty = value;
    }

    public boolean isSetAlteprty() {
        return (this.alteprty!= null);
    }

}

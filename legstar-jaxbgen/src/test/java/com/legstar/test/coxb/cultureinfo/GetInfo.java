
package com.legstar.test.coxb.cultureinfo;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for getInfo complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="getInfo">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="arg0" type="{http://cultureinfo.cases.test.xsdc.legstar.com/}cultureInfoParameters" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "getInfo", propOrder = {
    "arg0"
})
public class GetInfo
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @CobolElement(cobolName = "arg0", type = CobolType.GROUP_ITEM, levelNumber = 3)
    protected CultureInfoParameters arg0;

    /**
     * Gets the value of the arg0 property.
     * 
     * @return
     *     possible object is
     *     {@link CultureInfoParameters }
     *     
     */
    public CultureInfoParameters getArg0() {
        return arg0;
    }

    /**
     * Sets the value of the arg0 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CultureInfoParameters }
     *     
     */
    public void setArg0(CultureInfoParameters value) {
        this.arg0 = value;
    }

    public boolean isSetArg0() {
        return (this.arg0 != null);
    }

}

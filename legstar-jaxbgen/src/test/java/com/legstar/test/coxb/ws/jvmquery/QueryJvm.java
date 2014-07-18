
package com.legstar.test.coxb.ws.jvmquery;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for queryJvm complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="queryJvm">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="arg0" type="{http://jvmquery.ws.cases.test.xsdc.legstar.com/}jvmQueryRequest" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "queryJvm", propOrder = {
    "arg0"
})
public class QueryJvm
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @CobolElement(cobolName = "arg0", type = CobolType.GROUP_ITEM, levelNumber = 3)
    protected JvmQueryRequest arg0;

    /**
     * Gets the value of the arg0 property.
     * 
     * @return
     *     possible object is
     *     {@link JvmQueryRequest }
     *     
     */
    public JvmQueryRequest getArg0() {
        return arg0;
    }

    /**
     * Sets the value of the arg0 property.
     * 
     * @param value
     *     allowed object is
     *     {@link JvmQueryRequest }
     *     
     */
    public void setArg0(JvmQueryRequest value) {
        this.arg0 = value;
    }

    public boolean isSetArg0() {
        return (this.arg0 != null);
    }

}

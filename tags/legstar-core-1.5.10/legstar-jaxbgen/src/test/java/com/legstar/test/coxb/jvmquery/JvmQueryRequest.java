
package com.legstar.test.coxb.jvmquery;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolComplexType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for jvmQueryRequest complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="jvmQueryRequest">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="envVarNames" type="{http://www.w3.org/2001/XMLSchema}string" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "jvmQueryRequest", propOrder = {
    "envVarNames"
})
@CobolComplexType(javaClassName = "com.legstar.xsdc.test.cases.jvmquery.JVMQueryRequest")
public class JvmQueryRequest
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(nillable = true)
    @CobolElement(cobolName = "envVarNames", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 3, minOccurs = 0, maxOccurs = 10, picture = "X(32)", usage = "DISPLAY")
    protected List<String> envVarNames;

    /**
     * Gets the value of the envVarNames property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the envVarNames property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getEnvVarNames().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link String }
     * 
     * 
     */
    public List<String> getEnvVarNames() {
        if (envVarNames == null) {
            envVarNames = new ArrayList<String>();
        }
        return this.envVarNames;
    }

    public boolean isSetEnvVarNames() {
        return ((this.envVarNames!= null)&&(!this.envVarNames.isEmpty()));
    }

    public void unsetEnvVarNames() {
        this.envVarNames = null;
    }

}

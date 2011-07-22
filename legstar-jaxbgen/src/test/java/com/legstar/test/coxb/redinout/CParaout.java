
package com.legstar.test.coxb.redinout;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for CParaout complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="CParaout">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="CSomeOutput">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedInt">
 *               &lt;totalDigits value="8"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="Filler27">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="17"/>
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
@XmlType(name = "CParaout", propOrder = {
    "cSomeOutput",
    "filler27"
})
public class CParaout
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(name = "CSomeOutput")
    @CobolElement(cobolName = "C-SOME-OUTPUT", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 10, isSigned = false, totalDigits = 8, picture = "9(8)", srceLine = 26)
    protected long cSomeOutput;
    @XmlElement(name = "Filler27", required = true)
    @CobolElement(cobolName = "FILLER", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 10, picture = "X(17)", srceLine = 27)
    protected String filler27;

    /**
     * Gets the value of the cSomeOutput property.
     * 
     */
    public long getCSomeOutput() {
        return cSomeOutput;
    }

    /**
     * Sets the value of the cSomeOutput property.
     * 
     */
    public void setCSomeOutput(long value) {
        this.cSomeOutput = value;
    }

    public boolean isSetCSomeOutput() {
        return true;
    }

    /**
     * Gets the value of the filler27 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFiller27() {
        return filler27;
    }

    /**
     * Sets the value of the filler27 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFiller27(String value) {
        this.filler27 = value;
    }

    public boolean isSetFiller27() {
        return (this.filler27 != null);
    }

}

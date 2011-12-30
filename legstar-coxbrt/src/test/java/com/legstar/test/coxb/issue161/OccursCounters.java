
package com.legstar.test.coxb.issue161;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;


/**
 * <p>Java class for OccursCounters complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="OccursCounters">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="table1Ctr">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *               &lt;totalDigits value="3"/>
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
@XmlType(name = "OccursCounters", propOrder = {
    "table1Ctr"
})
public class OccursCounters
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @CobolElement(cobolName = "TABLE1-CTR", type = CobolType.ZONED_DECIMAL_ITEM, levelNumber = 10, isSigned = false, totalDigits = 3, picture = "999", srceLine = 3)
    protected int table1Ctr;

    /**
     * Gets the value of the table1Ctr property.
     * 
     */
    public int getTable1Ctr() {
        return table1Ctr;
    }

    /**
     * Sets the value of the table1Ctr property.
     * 
     */
    public void setTable1Ctr(int value) {
        this.table1Ctr = value;
    }

    public boolean isSetTable1Ctr() {
        return true;
    }

}

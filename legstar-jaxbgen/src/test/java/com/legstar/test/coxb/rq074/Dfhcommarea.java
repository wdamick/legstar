
package com.legstar.test.coxb.rq074;

import java.io.Serializable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
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
 *         &lt;element name="rq074Crud">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *               &lt;maxLength value="3"/>
 *               &lt;enumeration value="UPD"/>
 *               &lt;enumeration value="INQ"/>
 *               &lt;enumeration value="CRE"/>
 *               &lt;enumeration value="DEL"/>
 *             &lt;/restriction>
 *           &lt;/simpleType>
 *         &lt;/element>
 *         &lt;element name="rq074Bool">
 *           &lt;simpleType>
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
 *               &lt;totalDigits value="4"/>
 *               &lt;enumeration value="1"/>
 *               &lt;enumeration value="0"/>
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
    "rq074Crud",
    "rq074Bool"
})
public class Dfhcommarea
    implements Serializable
{

    private final static long serialVersionUID = 1L;
    @XmlElement(required = true)
    @CobolElement(cobolName = "RQ074-CRUD", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 5, picture = "X(03)", srceLine = 2)
    protected Dfhcommarea.Rq074Crud rq074Crud;
    @XmlElement(required = true)
    @CobolElement(cobolName = "RQ074-BOOL", type = CobolType.BINARY_ITEM, levelNumber = 5, isSigned = false, totalDigits = 4, picture = "9(04)", usage = "BINARY", srceLine = 7)
    protected Dfhcommarea.Rq074Bool rq074Bool;

    /**
     * Gets the value of the rq074Crud property.
     * 
     * @return
     *     possible object is
     *     {@link Dfhcommarea.Rq074Crud }
     *     
     */
    public Dfhcommarea.Rq074Crud getRq074Crud() {
        return rq074Crud;
    }

    /**
     * Sets the value of the rq074Crud property.
     * 
     * @param value
     *     allowed object is
     *     {@link Dfhcommarea.Rq074Crud }
     *     
     */
    public void setRq074Crud(Dfhcommarea.Rq074Crud value) {
        this.rq074Crud = value;
    }

    public boolean isSetRq074Crud() {
        return (this.rq074Crud!= null);
    }

    /**
     * Gets the value of the rq074Bool property.
     * 
     * @return
     *     possible object is
     *     {@link Dfhcommarea.Rq074Bool }
     *     
     */
    public Dfhcommarea.Rq074Bool getRq074Bool() {
        return rq074Bool;
    }

    /**
     * Sets the value of the rq074Bool property.
     * 
     * @param value
     *     allowed object is
     *     {@link Dfhcommarea.Rq074Bool }
     *     
     */
    public void setRq074Bool(Dfhcommarea.Rq074Bool value) {
        this.rq074Bool = value;
    }

    public boolean isSetRq074Bool() {
        return (this.rq074Bool!= null);
    }


    /**
     * <p>Java class for null.
     * 
     * <p>The following schema fragment specifies the expected content contained within this class.
     * <p>
     * <pre>
     * &lt;simpleType>
     *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}unsignedShort">
     *     &lt;totalDigits value="4"/>
     *     &lt;enumeration value="1"/>
     *     &lt;enumeration value="0"/>
     *   &lt;/restriction>
     * &lt;/simpleType>
     * </pre>
     * 
     */
    @XmlType(name = "")
    @XmlEnum(Integer.class)
    public enum Rq074Bool {

        @XmlEnumValue("1")
        VALUE_1(1),
        @XmlEnumValue("0")
        VALUE_0(0);
        private final int value;

        Rq074Bool(int v) {
            value = v;
        }

        public int value() {
            return value;
        }

        public static Dfhcommarea.Rq074Bool fromValue(int v) {
            for (Dfhcommarea.Rq074Bool c: Dfhcommarea.Rq074Bool.values()) {
                if (c.value == v) {
                    return c;
                }
            }
            throw new IllegalArgumentException(String.valueOf(v));
        }

    }


    /**
     * <p>Java class for null.
     * 
     * <p>The following schema fragment specifies the expected content contained within this class.
     * <p>
     * <pre>
     * &lt;simpleType>
     *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
     *     &lt;maxLength value="3"/>
     *     &lt;enumeration value="UPD"/>
     *     &lt;enumeration value="INQ"/>
     *     &lt;enumeration value="CRE"/>
     *     &lt;enumeration value="DEL"/>
     *   &lt;/restriction>
     * &lt;/simpleType>
     * </pre>
     * 
     */
    @XmlType(name = "")
    @XmlEnum
    public enum Rq074Crud {

        @XmlEnumValue("UPD")
        VALUE_UPD("UPD"),
        @XmlEnumValue("INQ")
        VALUE_INQ("INQ"),
        @XmlEnumValue("CRE")
        VALUE_CRE("CRE"),
        @XmlEnumValue("DEL")
        VALUE_DEL("DEL");
        private final String value;

        Rq074Crud(String v) {
            value = v;
        }

        public String value() {
            return value;
        }

        public static Dfhcommarea.Rq074Crud fromValue(String v) {
            for (Dfhcommarea.Rq074Crud c: Dfhcommarea.Rq074Crud.values()) {
                if (c.value.equals(v)) {
                    return c;
                }
            }
            throw new IllegalArgumentException(v);
        }

    }

}

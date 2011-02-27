
package com.legstar.test.coxb.MSNSearch;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for SafeSearchOptions.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="SafeSearchOptions">
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *     &lt;enumeration value="Moderate"/>
 *     &lt;enumeration value="Strict"/>
 *     &lt;enumeration value="Off"/>
 *   &lt;/restriction>
 * &lt;/simpleType>
 * </pre>
 * 
 */
@XmlType(name = "SafeSearchOptions")
@XmlEnum
public enum SafeSearchOptionsType {

    @XmlEnumValue("Moderate")
    MODERATE("Moderate"),
    @XmlEnumValue("Strict")
    STRICT("Strict"),
    @XmlEnumValue("Off")
    OFF("Off");
    private final String value;

    SafeSearchOptionsType(String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    public static SafeSearchOptionsType fromValue(String v) {
        for (SafeSearchOptionsType c: SafeSearchOptionsType.values()) {
            if (c.value.equals(v)) {
                return c;
            }
        }
        throw new IllegalArgumentException(v);
    }

}

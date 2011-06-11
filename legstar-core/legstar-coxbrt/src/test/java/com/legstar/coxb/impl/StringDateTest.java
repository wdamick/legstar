package com.legstar.coxb.impl;

import java.io.Serializable;
import java.util.Date;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;

import junit.framework.TestCase;

import com.legstar.coxb.CobolComplexType;
import com.legstar.coxb.CobolElement;
import com.legstar.coxb.CobolType;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.impl.visitor.CobolMarshalVisitor;
import com.legstar.coxb.impl.visitor.CobolUnmarshalVisitor;

/**
 * Test that java.util.Date can be handled as a String
 * 
 */
public class StringDateTest extends TestCase {

    /**
     * Creates an instance of the annotated POJO then run it through the COBOL
     * marshaler using reflection.
     * 
     * @throws Exception if test fails.
     */
    public void testDateToHost() throws Exception {

        MyDateClass valueObject = new MyDateClass();
        valueObject.setFieldString1("A1");
        valueObject.setFieldDate(new Date(1307773945303L));
        valueObject.setFieldString2("A2");

        byte[] hostBytes = new byte[36];
        CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0,
                new CobolSimpleConverters());

        // Traverse the object structure, visiting each node with the
        // visitor
        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new ObjectFactory(), valueObject);
        ccem.accept(mv);
        assertEquals(
                "f2f0f1f160f0f660f1f140f0f87af3f27af2f54bf3f0f3404040404040404040c1f1c1f2",
                HostData.toHexString(hostBytes));
    }

    /**
     * Creates an instance of the annotated POJO then run it through the COBOL
     * marshaler using reflection.
     * 
     * @throws Exception if test fails.
     */
    public void testHostToDate() throws Exception {

        byte[] hostBytes = HostData
                .toByteArray("f2f0f1f160f0f660f1f140f0f87af3f27af2f54bf3f0f3404040404040404040c1f1c1f2");
        CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0,
                new CobolSimpleConverters());

        // Traverse the object structure, visiting each node with the
        // visitor
        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new ObjectFactory(), MyDateClass.class);
        ccem.accept(uv);

        MyDateClass valueObject = (MyDateClass) ccem
                .getObjectValue(MyDateClass.class);
        assertEquals("A1", valueObject.getFieldString1());
        assertEquals("A2", valueObject.getFieldString2());
        assertEquals("Sat Jun 11 08:32:25 CEST 2011", valueObject
                .getFieldDate().toString());
    }

    /**
     * Mock object factory similar to JAXB one.
     * 
     */
    protected class ObjectFactory {
        /**
         * Create an instance of {@link MyDateClass }
         * 
         */
        public MyDateClass createStringDateTest$MyDateClass() {
            return new MyDateClass();
        }
    }

    /**
     * POJO with JAXB and LegStar annotations.
     * 
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "myDateClass", propOrder = { "fieldDate", "fieldString1",
            "fieldString2" })
    @CobolComplexType(javaClassName = "com.legsem.test.MyDateClass")
    protected class MyDateClass implements Serializable {

        private final static long serialVersionUID = 1L;
        @XmlElement(type = String.class)
        @XmlSchemaType(name = "dateTime")
        @CobolElement(cobolName = "fieldDate", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 3, picture = "X(32)", usage = "DISPLAY")
        protected Date fieldDate;
        @CobolElement(cobolName = "fieldString1", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 3, picture = "X(2)", usage = "DISPLAY")
        protected String fieldString1;
        @CobolElement(cobolName = "fieldString2", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 3, picture = "X(2)", usage = "DISPLAY")
        protected String fieldString2;

        /**
         * Gets the value of the fieldDate property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public Date getFieldDate() {
            return fieldDate;
        }

        /**
         * Sets the value of the fieldDate property.
         * 
         * @param value allowed object is {@link String }
         * 
         */
        public void setFieldDate(Date value) {
            this.fieldDate = value;
        }

        public boolean isSetFieldDate() {
            return (this.fieldDate != null);
        }

        /**
         * Gets the value of the fieldString1 property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getFieldString1() {
            return fieldString1;
        }

        /**
         * Sets the value of the fieldString1 property.
         * 
         * @param value allowed object is {@link String }
         * 
         */
        public void setFieldString1(String value) {
            this.fieldString1 = value;
        }

        public boolean isSetFieldString1() {
            return (this.fieldString1 != null);
        }

        /**
         * Gets the value of the fieldString2 property.
         * 
         * @return possible object is {@link String }
         * 
         */
        public String getFieldString2() {
            return fieldString2;
        }

        /**
         * Sets the value of the fieldString2 property.
         * 
         * @param value allowed object is {@link String }
         * 
         */
        public void setFieldString2(String value) {
            this.fieldString2 = value;
        }

        public boolean isSetFieldString2() {
            return (this.fieldString2 != null);
        }

    }

}

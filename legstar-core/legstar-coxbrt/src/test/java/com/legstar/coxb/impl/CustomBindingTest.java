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
import com.legstar.coxb.CobolJavaTypeAdapter;
import com.legstar.coxb.CobolType;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.impl.visitor.CobolMarshalVisitor;
import com.legstar.coxb.impl.visitor.CobolUnmarshalVisitor;

/**
 * Test that @CobolJavaTypeAdapter annotation for custom binding.
 * 
 */
public class CustomBindingTest extends TestCase {

    /**
     * Creates an instance of the annotated POJO then run it through the COBOL
     * marshaler using reflection.
     * 
     * @throws Exception if test fails.
     */
    public void testDateToHost() throws Exception {

        MyCustomClass valueObject = new MyCustomClass();
        valueObject.setFieldDate(new Date(1307773945303L));

        byte[] hostBytes = new byte[32];
        CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0,
                new CobolSimpleConverters());

        // Traverse the object structure, visiting each node with the
        // visitor
        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new ObjectFactory(), valueObject);
        ccem.accept(mv);
        assertEquals(
                "f2f0f1f160f0f660f1f140f0f87af3f27af2f54bf3f0f3404040404040404040",
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
                .toByteArray("f2f0f1f160f0f660f1f140f0f87af3f27af2f54bf3f0f3404040404040404040");
        CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0,
                new CobolSimpleConverters());

        // Traverse the object structure, visiting each node with the
        // visitor
        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new ObjectFactory(), MyCustomClass.class);
        ccem.accept(uv);

        MyCustomClass valueObject = (MyCustomClass) ccem
                .getObjectValue(MyCustomClass.class);
        assertEquals("Sat Jun 11 08:32:25 CEST 2011", valueObject
                .getFieldDate().toString());
    }

    /**
     * Mock object factory similar to JAXB one.
     * 
     */
    protected class ObjectFactory {
        /**
         * Create an instance of {@link MyCustomClass }
         * 
         */
        public MyCustomClass createCustomBindingTest$MyCustomClass() {
            return new MyCustomClass();
        }
    }

    /**
     * POJO with JAXB and LegStar annotations. Uses @CobolJavaTypeAdapter to
     * provide a custom binding for a field
     * 
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "MyCustomClass", propOrder = { "fieldDate" })
    @CobolComplexType(javaClassName = "com.legsem.test.MyCustomClass")
    protected class MyCustomClass implements Serializable {

        private final static long serialVersionUID = 1L;
        @XmlElement(type = String.class)
        @XmlSchemaType(name = "dateTime")
        @CobolJavaTypeAdapter(value = CustomBinding.class)
        @CobolElement(cobolName = "fieldDate", type = CobolType.ALPHANUMERIC_ITEM, levelNumber = 3, picture = "X(32)", usage = "DISPLAY")
        protected Date fieldDate;

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

    }

    public class CustomBinding extends CStringBinding {

        public CustomBinding(String bindingName, String jaxbName,
                Class < ? > jaxbType, CobolElement cobolAnnotations,
                ICobolComplexBinding parentBinding) {
            super(bindingName, jaxbName, jaxbType, cobolAnnotations,
                    parentBinding);
            // TODO Auto-generated constructor stub
        }

    }
}

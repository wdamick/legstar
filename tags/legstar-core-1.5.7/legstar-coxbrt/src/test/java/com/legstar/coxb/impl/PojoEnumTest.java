package com.legstar.coxb.impl;

import junit.framework.TestCase;

import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.impl.visitor.CobolMarshalVisitor;
import com.legstar.coxb.impl.visitor.CobolUnmarshalVisitor;
import com.legstar.test.coxb.issue154.pojo.EnumContainerClass;

public class PojoEnumTest extends TestCase {

    public void testToHost() throws Exception {
        EnumContainerClass valueObject = new EnumContainerClass();
        valueObject.setDay(EnumContainerClass.Day.FRIDAY);

        byte[] hostBytes = new byte[32];
        CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0,
                new CobolSimpleConverters());

        // Pass the JAXB object for reflection but then set the POJO as the
        // value object
        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new com.legstar.test.coxb.issue154.ObjectFactory(),
                new com.legstar.test.coxb.issue154.EnumContainerClass());
        ccem.setObjectValue(valueObject);
        ccem.accept(mv);

        assertEquals(
                "c6d9c9c4c1e84040404040404040404040404040404040404040404040404040",
                HostData.toHexString(hostBytes));
    }

    public void testFromHost() throws Exception {
        byte[] hostBytes = HostData
                .toByteArray("c6d9c9c4c1e84040404040404040404040404040404040404040404040404040");
        CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0,
                new CobolSimpleConverters());

        // Traverse the object structure, visiting each node with the
        // visitor
        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new ObjectFactory(),
                com.legstar.test.coxb.issue154.EnumContainerClass.class);
        ccem.accept(uv);

        EnumContainerClass valueObject = (EnumContainerClass) ccem
                .getObjectValue(EnumContainerClass.class);

        assertEquals("FRIDAY", valueObject.getDay().toString());

    }

    protected class ObjectFactory {
        public EnumContainerClass createEnumContainerClass() {
            return new EnumContainerClass();
        }
    }
}

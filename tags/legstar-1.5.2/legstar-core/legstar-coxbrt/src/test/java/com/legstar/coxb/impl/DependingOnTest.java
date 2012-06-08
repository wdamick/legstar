package com.legstar.coxb.impl;

import junit.framework.TestCase;

import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.impl.visitor.CobolMarshalVisitor;
import com.legstar.coxb.impl.visitor.CobolUnmarshalVisitor;
import com.legstar.test.coxb.issue161.Dfhcommarea;
import com.legstar.test.coxb.issue161.ObjectFactory;
import com.legstar.test.coxb.issue161.OccursCounters;
import com.legstar.test.coxb.issue161.OccursTables;
import com.legstar.test.coxb.issue161.Table1;

public class DependingOnTest extends TestCase {

    public void testDependingOnToHost() throws Exception {
        Dfhcommarea dfhcommarea = getDfhcommarea();

        byte[] hostBytes = new byte[13];
        CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0,
                new CobolSimpleConverters());

        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new ObjectFactory(), dfhcommarea);
        ccem.accept(mv);
        assertEquals("f0f0f4f0f1f2f3000000000000",
                HostData.toHexString(hostBytes));

    }

    public void testHostToDependingOn() throws Exception {

        byte[] hostBytes = HostData.toByteArray("f0f0f5f0f1f2f3f4");
        CobolUnmarshalVisitor mv = new CobolUnmarshalVisitor(hostBytes, 0,
                new CobolSimpleConverters());

        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new ObjectFactory(), Dfhcommarea.class);
        ccem.accept(mv);
        Dfhcommarea dfhcommarea = (Dfhcommarea) ccem
                .getObjectValue(Dfhcommarea.class);
        assertEquals(5, dfhcommarea.getOccursCounters().getTable1Ctr());
        for (int i = 0; i < 5; i++) {
            assertEquals(Integer.toString(i), dfhcommarea.getOccursTables()
                    .getTable1().get(i).getItem());
        }

    }

    protected Dfhcommarea getDfhcommarea() {
        Dfhcommarea dfhcommarea = new Dfhcommarea();
        OccursCounters counters = new OccursCounters();
        counters.setTable1Ctr(4);
        dfhcommarea.setOccursCounters(counters);
        OccursTables tables = new OccursTables();
        for (int i = 0; i < 4; i++) {
            Table1 table1 = new Table1();
            table1.setItem(Integer.toString(i));
            tables.getTable1().add(table1);
        }
        dfhcommarea.setOccursTables(tables);
        return dfhcommarea;
    }

}

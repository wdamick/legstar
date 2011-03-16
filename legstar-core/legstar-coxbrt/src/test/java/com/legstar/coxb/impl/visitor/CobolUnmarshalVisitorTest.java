package com.legstar.coxb.impl.visitor;

import java.util.Hashtable;

import junit.framework.TestCase;

import com.legstar.coxb.CobolElementVisitor;
import com.legstar.coxb.ICobolBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.ICobolUnmarshalChoiceStrategy;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.test.coxb.rdef128.A;

/**
 * There are several unmarshaling tests elsewhere in this project. Here we focus
 * on special cases.
 * 
 */
public class CobolUnmarshalVisitorTest extends TestCase {

    /**
     * Tests related to http://code.google.com/p/legstar/issues/detail?id=128.
     * 
     * @throws Exception if unmarshaling fails
     */
    public void testVirtualFillerLargest() throws Exception {
        /* First alternative is the largest */
        byte[] hostBytes = HostData.toByteArray("C1C2C3C4C1C2C3C440400123652d");
        CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0,
                new CobolSimpleConverters());

        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new com.legstar.test.coxb.rdef128.ObjectFactory(), A.class);
        ccem.accept(uv);

        A a = (A) ccem.getObjectValue(A.class);

        assertEquals("ABCDABCD", a.getAlt1().getB());
        assertEquals(null, a.getAlt2());
        assertEquals("-1236.52", a.getD().toString());

    }

    /**
     * Tests related to http://code.google.com/p/legstar/issues/detail?id=128.
     * 
     * @throws Exception if unmarshaling fails
     */
    public void testVirtualFillerShortest() throws Exception {
        /* Second alternative is the shortes */
        byte[] hostBytes = HostData.toByteArray("0012345f4040404040400123652d");
        CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0,
                new CobolSimpleConverters());

        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new com.legstar.test.coxb.rdef128.ObjectFactory(), A.class);
        /* Now force the choice to pickup the second alternative */
        ICobolChoiceBinding choice = (ICobolChoiceBinding) ccem
                .getChildrenList().get(0);
        choice.setUnmarshalChoiceStrategy(new ICobolUnmarshalChoiceStrategy() {

            public ICobolBinding choose(ICobolChoiceBinding choice,
                    Hashtable < String, Object > variablesMap,
                    CobolElementVisitor visitor) throws HostException {
                return choice.getAlternativeByName("Alt2");
            }

        });
        ccem.accept(uv);

        A a = (A) ccem.getObjectValue(A.class);

        assertEquals(null, a.getAlt1());
        assertEquals("123.45", a.getAlt2().getC().toString());
        assertEquals("-1236.52", a.getD().toString());

    }
}

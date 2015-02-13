/*******************************************************************************
 * Copyright (c) 2011 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
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
import com.legstar.test.coxb.coxb137.BoolPojo;
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

    /**
     * Tests related to http://code.google.com/p/legstar/issues/detail?id=137.
     * 
     * @throws Exception if unmarshaling fails
     */
    public void testBooleanField() throws Exception {
        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new com.legstar.test.coxb.coxb137.ObjectFactory(),
                BoolPojo.class);
        assertEquals(26, ccem.getByteLength());

        byte[] hostBytes = HostData.toByteArray("0000000100000001");
        CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0,
                new CobolSimpleConverters());
        ccem.accept(uv);

        BoolPojo boolPojo = (BoolPojo) ccem.getObjectValue(BoolPojo.class);
        assertEquals(1, boolPojo.getBooleanList().size());
        assertFalse(boolPojo.getBooleanList().get(0));
        assertTrue(boolPojo.isABoolean());

    }

    /**
     * Tests related to http://code.google.com/p/legstar/issues/detail?id=186.
     * 
     * @throws Exception if unmarshaling fails
     */
    public void testIssue186() throws Exception {
        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new com.legstar.test.coxb.issue186.ObjectFactory(),
                com.legstar.test.coxb.issue186.Ardo02Record.class);
        assertEquals(7, ccem.getByteLength());

        byte[] hostBytes = HostData.toByteArray("0001c1");
        CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0,
                new CobolSimpleConverters());
        ccem.accept(uv);

        com.legstar.test.coxb.issue186.Ardo02Record ardo02Record = (com.legstar.test.coxb.issue186.Ardo02Record) ccem
                .getObjectValue(com.legstar.test.coxb.issue186.Ardo02Record.class);
        assertEquals(1, ardo02Record.getAlternativeA().getOdoCounter());
        assertEquals("A", ardo02Record.getOdoArray().get(0).getFiller10());

    }

    /**
     * Tests related to http://code.google.com/p/legstar/issues/detail?id=187.
     * 
     * @throws Exception if unmarshaling fails
     */
    public void testIssue187() throws Exception {
        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new com.legstar.test.coxb.issue187.ObjectFactory(),
                com.legstar.test.coxb.issue187.Ardo03Record.class);
        assertEquals(120, ccem.getByteLength());

        byte[] hostBytes = HostData.toByteArray("f0f0f0f0f1f0f0f1c1c2c3c4");
        CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(hostBytes, 0,
                new CobolSimpleConverters());
        ccem.accept(uv);

        com.legstar.test.coxb.issue187.Ardo03Record ardo03Record = (com.legstar.test.coxb.issue187.Ardo03Record) ccem
                .getObjectValue(com.legstar.test.coxb.issue187.Ardo03Record.class);
        assertEquals(1, ardo03Record.getOdoCounter());
        assertEquals(1, ardo03Record.getOdoArray().get(0).getOdoSubCounter());
        assertEquals("ABCD", ardo03Record.getOdoArray().get(0).getOdoSubArray()
                .get(0).getFiller8());

    }

}

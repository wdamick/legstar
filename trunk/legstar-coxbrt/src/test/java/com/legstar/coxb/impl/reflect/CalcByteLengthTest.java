package com.legstar.coxb.impl.reflect;

import com.legstar.coxb.host.HostException;
import com.legstar.test.coxb.ArraysdoCases;
import com.legstar.test.coxb.DplarchtCases;

import junit.framework.TestCase;

/**
 * Test that complex types maximum byte length is calculated correctly.
 */
public class CalcByteLengthTest extends TestCase {
    
    /**
     * Dplarcht has many combinations of complex elements, arrays and redefines.
     */
    public void testDplarcht() {
        try {
            CComplexReflectBinding ccem = new CComplexReflectBinding(
                    DplarchtCases.getFactory(), DplarchtCases.getJavaObject().getClass());
            assertEquals(0, ccem.getByteLength());
            assertEquals(32025, ccem.calcByteLength());
            assertEquals(32025, ccem.getByteLength());
        } catch (ReflectBindingException e) {
            fail(e.getMessage());
        } catch (HostException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Arraysdo has a variable size array of a simple type.
     */
    public void testArraysdo() {
        try {
            CComplexReflectBinding ccem = new CComplexReflectBinding(
                    ArraysdoCases.getFactory(), ArraysdoCases.getJavaObject().getClass());
            assertEquals(0, ccem.getByteLength());
            assertEquals(502, ccem.calcByteLength());
            assertEquals(502, ccem.getByteLength());
        } catch (ReflectBindingException e) {
            fail(e.getMessage());
        } catch (HostException e) {
            fail(e.getMessage());
        }
    }
}

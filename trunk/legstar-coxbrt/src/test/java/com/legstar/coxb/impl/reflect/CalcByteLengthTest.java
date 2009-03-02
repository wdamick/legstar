package com.legstar.coxb.impl.reflect;

import com.legstar.test.coxb.ArraysdoCases;
import com.legstar.test.coxb.DplarchtCases;
import com.legstar.test.coxb.FixarnumCases;
import com.legstar.test.coxb.LsfileaeCases;

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
            assertEquals(32025, ccem.getByteLength());
        } catch (ReflectBindingException e) {
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
            assertEquals(502, ccem.getByteLength());
        } catch (ReflectBindingException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Fixarnum has a various array types.
     */
    public void testFixarnum() {
        try {
            CComplexReflectBinding ccem = new CComplexReflectBinding(
                    FixarnumCases.getFactory(), FixarnumCases.getJavaObject().getClass());
            assertEquals(78, ccem.getByteLength());
        } catch (ReflectBindingException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Lsfileae case.
     */
    public void testLsfileae() {
        try {
            CComplexReflectBinding ccem = new CComplexReflectBinding(
                    LsfileaeCases.getFactory(), LsfileaeCases.getJavaObject().getClass());
            assertEquals(79, ccem.getByteLength());
        } catch (ReflectBindingException e) {
            fail(e.getMessage());
        }
    }
}

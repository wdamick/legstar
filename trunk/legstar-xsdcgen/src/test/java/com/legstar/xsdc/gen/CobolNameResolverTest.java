package com.legstar.xsdc.gen;

import com.legstar.xsdc.gen.CobolNameResolver;

import junit.framework.TestCase;

public class CobolNameResolverTest extends TestCase {
	
	public void testSimple() throws Exception {
		CobolNameResolver cnr = new CobolNameResolver();
		assertEquals("var", cnr.getName("var"));
	}

	public void testInvalidChars() throws Exception {
		CobolNameResolver cnr = new CobolNameResolver();
		assertEquals("varTO75-TO01", cnr.getName("9__var@TO75_TO01__"));
	}

	public void testTruncation() throws Exception {
		CobolNameResolver cnr = new CobolNameResolver();
		assertEquals("T12345678901234567890123456789", cnr.getName("T123456789012345678901234567890"));
		assertEquals("T1234567890123456789012345678", cnr.getName("T1234567890123456789012345678-0"));
	}

	public void testReservedWord() throws Exception {
		CobolNameResolver cnr = new CobolNameResolver();
		assertEquals("R-count", cnr.getName("count"));
	}

	public void testMakeUnique() throws Exception {
		CobolNameResolver cnr = new CobolNameResolver();
		assertEquals("Var1", cnr.getUniqueName("Var1"));
		assertEquals("Var10", cnr.getUniqueName("Var1"));
		assertEquals("Var11", cnr.getUniqueName("Var1"));
	}

	public void testMakeUniqueWithLongNames() throws Exception {
		CobolNameResolver cnr = new CobolNameResolver();
		assertEquals("T12345678901234567890123456780", cnr.getUniqueName("T12345678901234567890123456780"));
		assertEquals("T12345678901234567890123456781", cnr.getUniqueName("T12345678901234567890123456781"));
		assertEquals("T12345678901234567890123456782", cnr.getUniqueName("T12345678901234567890123456782"));
		assertEquals("T12345678901234567890123456783", cnr.getUniqueName("T12345678901234567890123456783"));
		assertEquals("T12345678901234567890123456784", cnr.getUniqueName("T12345678901234567890123456784"));
		assertEquals("T12345678901234567890123456785", cnr.getUniqueName("T12345678901234567890123456785"));
		assertEquals("T12345678901234567890123456786", cnr.getUniqueName("T12345678901234567890123456786"));
		assertEquals("T12345678901234567890123456787", cnr.getUniqueName("T12345678901234567890123456787"));
		assertEquals("T12345678901234567890123456788", cnr.getUniqueName("T12345678901234567890123456788"));
		assertEquals("T12345678901234567890123456789", cnr.getUniqueName("T12345678901234567890123456789"));
		assertEquals("T12345678901234567890123456710", cnr.getUniqueName("T12345678901234567890123456780"));
	}
}

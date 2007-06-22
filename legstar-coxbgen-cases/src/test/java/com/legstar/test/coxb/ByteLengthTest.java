package com.legstar.test.coxb;


import junit.framework.TestCase;

public class ByteLengthTest extends TestCase {
	
	public void testFixarsim() throws Exception {
		assertEquals(15, Util.getByteLength("fixarsim"));
	}

	public void testLsfileae() throws Exception {
		assertEquals(79, Util.getByteLength("lsfileae"));
	}

	public void testRedsimpt() throws Exception {
		assertEquals(18, Util.getByteLength("redsimpt"));
	}

	public void testVararcom() throws Exception {
		assertEquals(1752, Util.getByteLength("vararcom"));
	}

	public void testBinarcht() throws Exception {
		assertEquals(56, Util.getByteLength("binarcht"));
	}

	public void testBinnatsi() throws Exception {
		assertEquals(56, Util.getByteLength("binnatsi"));
	}

	public void testDoublmix() throws Exception {
		assertEquals(48, Util.getByteLength("doublmix"));
	}

	public void testDplarcht() throws Exception {
		assertEquals(32025, Util.getByteLength("dplarcht"));
	}

	public void testFixarcom() throws Exception {
		assertEquals(49, Util.getByteLength("fixarcom"));
	}

	public void testFixarnum() throws Exception {
		assertEquals(78, Util.getByteLength("fixarnum"));
	}

	public void testRedbotha() throws Exception {
		assertEquals(2, Util.getByteLength("redbotha"));
	}
	public void testRedopera() throws Exception {
		assertEquals(218, Util.getByteLength("redopera"));
	}
	public void testTypesmix() throws Exception {
		assertEquals(176, Util.getByteLength("typesmix"));
	}

	public void testAlltypes() throws Exception {
		assertEquals(267, Util.getByteLength("alltypes"));
	}
	
	public void testRedinout() throws Exception {
		assertEquals(502, Util.getByteLength("redinout"));
	}
}

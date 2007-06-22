package com.legstar.coxb.convert.simple.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for com.legstar.coxb.convert.simple.test");
		//$JUnit-BEGIN$
		suite.addTestSuite(StringTest.class);
		suite.addTestSuite(FloatTest.class);
		suite.addTestSuite(BinaryTest.class);
		suite.addTestSuite(OctetStreamTest.class);
		suite.addTestSuite(DoubleTest.class);
		suite.addTestSuite(ByteLengthTest.class);
		suite.addTestSuite(ZonedDecimalTest.class);
		suite.addTestSuite(PackedDecimalTest.class);
		//$JUnit-END$
		return suite;
	}

}

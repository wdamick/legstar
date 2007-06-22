package com.legstar.coxb.convert.simple.perf.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for com.legstar.coxb.convert.simple.perf.test");
		//$JUnit-BEGIN$
		suite.addTestSuite(ZonedDecimalFromHostTest.class);
		suite.addTestSuite(BinaryToHostTest.class);
		suite.addTestSuite(StringFromHostTest.class);
		suite.addTestSuite(PackedDecimalToHostTest.class);
		suite.addTestSuite(PackedDecimalFromHostTest.class);
		suite.addTestSuite(ZonedDecimalToHostTest.class);
		suite.addTestSuite(StringlToHostTest.class);
		suite.addTestSuite(BinaryFromHostTest.class);
		//$JUnit-END$
		return suite;
	}

}

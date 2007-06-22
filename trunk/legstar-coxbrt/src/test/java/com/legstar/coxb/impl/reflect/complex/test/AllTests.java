package com.legstar.coxb.impl.reflect.complex.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for com.legstar.coxb.reflect.test");
		//$JUnit-BEGIN$
		suite.addTestSuite(MarshallerVisitorTest.class);
		suite.addTestSuite(UnmarshallerVisitorTest.class);
		//$JUnit-END$
		return suite;
	}

}

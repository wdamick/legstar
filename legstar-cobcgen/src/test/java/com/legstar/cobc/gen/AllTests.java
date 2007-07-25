package com.legstar.cobc.gen;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for com.legstar.cobc.gen");
		//$JUnit-BEGIN$
		suite.addTestSuite(CobolGeneratorTest.class);
		suite.addTestSuite(CobolGenSentenceTest.class);
		suite.addTestSuite(CobolGenVisitorTest.class);
		suite.addTestSuite(CobolGenFormatterTest.class);
		//$JUnit-END$
		return suite;
	}

}

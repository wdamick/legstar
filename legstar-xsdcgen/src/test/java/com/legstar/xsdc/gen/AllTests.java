package com.legstar.xsdc.gen;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for com.legstar.xsdc.xsdgen");
		//$JUnit-BEGIN$
		suite.addTestSuite(XsdCobolAnnotatorTest.class);
		suite.addTestSuite(CobolNameResolverTest.class);
		//$JUnit-END$
		return suite;
	}

}

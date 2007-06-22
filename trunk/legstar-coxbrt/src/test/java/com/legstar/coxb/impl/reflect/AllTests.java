package com.legstar.coxb.impl.reflect;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for com.legstar.coxb.impl.reflect");
		//$JUnit-BEGIN$
		suite.addTestSuite(UnmarshalArrayssmTest.class);
		suite.addTestSuite(UnmarshalRedsimptTest.class);
		suite.addTestSuite(MarshalArraysdoTest.class);
		suite.addTestSuite(MarshalRedsimptTest.class);
		suite.addTestSuite(UnmarshalLsfileaeTest.class);
		suite.addTestSuite(MarshalLsfileaeTest.class);
		suite.addTestSuite(MarshalArrayssmTest.class);
		suite.addTestSuite(UnmarshalArraysdoTest.class);
		//$JUnit-END$
		return suite;
	}

}

package com.legstar.coxb.impl.reflect;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for com.legstar.coxb.impl.reflect");
		//$JUnit-BEGIN$
		suite.addTestSuite(MarshalNumzonedTest.class);
		suite.addTestSuite(UnmarshalArrayssmTest.class);
		suite.addTestSuite(UnmarshalEnumvarTest.class);
		suite.addTestSuite(UnmarshalRedsimptTest.class);
		suite.addTestSuite(MarshalArraysdoTest.class);
		suite.addTestSuite(UnmarshalLsfileaeTest.class);
		suite.addTestSuite(MarshalEnumvarTest.class);
		suite.addTestSuite(UnmarshalArraysdoTest.class);
		suite.addTestSuite(MarshalListssdoTest.class);
		suite.addTestSuite(UnmarshalListssdoTest.class);
		suite.addTestSuite(MarshalArrayssmTest.class);
		suite.addTestSuite(UnmarshalNumzonedTest.class);
		suite.addTestSuite(MarshalRedsimptTest.class);
		suite.addTestSuite(MarshalLsfileaeTest.class);
		//$JUnit-END$
		return suite;
	}

}

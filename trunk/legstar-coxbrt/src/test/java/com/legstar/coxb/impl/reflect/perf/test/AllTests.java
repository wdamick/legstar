package com.legstar.coxb.impl.reflect.perf.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for com.legstar.coxb.reflect.perf.test");
		//$JUnit-BEGIN$
		suite.addTestSuite(UnmarshalSmallVolumeTest.class);
		suite.addTestSuite(MarshalSmallVolumeTest.class);
		suite.addTestSuite(UnmarshalLargeVolumeTest.class);
		suite.addTestSuite(MarshalMediumVolumeTest.class);
		suite.addTestSuite(UnmarshalMediumVolumeTest.class);
		suite.addTestSuite(MarshalLargeVolumeTest.class);
		//$JUnit-END$
		return suite;
	}

}

package com.legstar.host.invoke;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for com.legstar.host.invoke");
		//$JUnit-BEGIN$
		suite.addTestSuite(ContainerInvokerTest.class);
		suite.addTestSuite(CicsProgramTest.class);
		suite.addTestSuite(CommareaInvokerTest.class);
		suite.addTestSuite(HostInvokerFactoryTest.class);
		//$JUnit-END$
		return suite;
	}

}

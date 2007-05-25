package com.legstar.clients.tests;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for com.legstar.clients.tests");
		//$JUnit-BEGIN$
		suite.addTestSuite(ClientredmultiTest.class);
		suite.addTestSuite(ClientlsfileadTest.class);
		suite.addTestSuite(ClientbinarchtTest.class);
		suite.addTestSuite(ClientbinpkdusTest.class);
		suite.addTestSuite(ClientfixarcomTest.class);
		suite.addTestSuite(ClientdplarchtIterationTest.class);
		suite.addTestSuite(ClientredoperaTest.class);
		suite.addTestSuite(ClientlsfilealTest.class);
		suite.addTestSuite(ClientbinnatusTest.class);
		suite.addTestSuite(ClientfloatmixTest.class);
		suite.addTestSuite(ClientlsfileacTest.class);
		suite.addTestSuite(ClientlsfileaeIterationTest.class);
		suite.addTestSuite(ClientredinoutTest.class);
		suite.addTestSuite(ClientredsimptTest.class);
		suite.addTestSuite(ClientredbothaTest.class);
		suite.addTestSuite(ClientdoublmixTest.class);
		suite.addTestSuite(ClientfixarsimTest.class);
		suite.addTestSuite(ClientvararcomTest.class);
		suite.addTestSuite(ClientdplarchtTest.class);
		suite.addTestSuite(ClienttypesmixTest.class);
		suite.addTestSuite(ClientlsfileaeTest.class);
		suite.addTestSuite(ClientfixarnumTest.class);
		suite.addTestSuite(ClientbinnatsiTest.class);
		//$JUnit-END$
		return suite;
	}

}

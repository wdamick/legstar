/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.clients.tests;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for com.legstar.clients.tests");
		//$JUnit-BEGIN$
		suite.addTestSuite(ClientlsfileadTest.class);
		suite.addTestSuite(ClientfixarcomTest.class);
		suite.addTestSuite(ClientdplarchtIterationTest.class);
		suite.addTestSuite(ClientfloatmixTest.class);
		suite.addTestSuite(ClientlsfileacTest.class);
		suite.addTestSuite(ClientredinoutTest.class);
		suite.addTestSuite(ClientredbothaTest.class);
		suite.addTestSuite(ClientredsimptTest.class);
		suite.addTestSuite(ClientfixarsimTest.class);
		suite.addTestSuite(ClientvararcomTest.class);
		suite.addTestSuite(ClientNumzonedTest.class);
		suite.addTestSuite(ClientlsfileaeTest.class);
		suite.addTestSuite(ClientfixarnumTest.class);
		suite.addTestSuite(ClientbinnatsiTest.class);
		suite.addTestSuite(ClientredmultiTest.class);
		suite.addTestSuite(ClientbinarchtTest.class);
		suite.addTestSuite(ClientbinpkdusTest.class);
		suite.addTestSuite(ClientredoperaTest.class);
		suite.addTestSuite(ClientlsfilealTest.class);
		suite.addTestSuite(ClientbinnatusTest.class);
		suite.addTestSuite(ClientlsfileaeIterationTest.class);
		suite.addTestSuite(ClientdoublmixTest.class);
		suite.addTestSuite(ClientdplarchtTest.class);
		suite.addTestSuite(ClienttypesmixTest.class);
		//$JUnit-END$
		return suite;
	}

}

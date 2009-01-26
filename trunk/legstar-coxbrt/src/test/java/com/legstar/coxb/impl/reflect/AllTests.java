/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
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

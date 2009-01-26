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

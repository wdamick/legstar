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

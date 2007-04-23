/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.clients.tests;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for com.legstar.test.custom");
		//$JUnit-BEGIN$
		suite.addTestSuite(ClientredmultiTest.class);
		suite.addTestSuite(ClientfloatmixTest.class);
		suite.addTestSuite(ClientredinoutTest.class);
		suite.addTestSuite(ClientbinnatsiTest.class);
		suite.addTestSuite(ClientbinnatusTest.class);
		suite.addTestSuite(ClientredsimptTest.class);
		suite.addTestSuite(ClientbinarchtTest.class);
		suite.addTestSuite(ClientdplarchtTest.class);
		suite.addTestSuite(ClientredoperaTest.class);
		suite.addTestSuite(ClientdoublmixTest.class);
		suite.addTestSuite(ClientvararcomTest.class);
		suite.addTestSuite(ClientfixarsimTest.class);
		suite.addTestSuite(ClientbinpkdusTest.class);
		suite.addTestSuite(ClienttypesmixTest.class);
		suite.addTestSuite(ClientlsfileadTest.class);
		suite.addTestSuite(ClientfixarcomTest.class);
		suite.addTestSuite(ClientfixarnumTest.class);
		suite.addTestSuite(ClientlsfileaeTest.class);
		suite.addTestSuite(ClientredbothaTest.class);
		//$JUnit-END$
		return suite;
	}

}

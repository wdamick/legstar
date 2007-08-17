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
package com.legstar.coxb.convert.simple.perf.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for com.legstar.coxb.convert.simple.perf.test");
		//$JUnit-BEGIN$
		suite.addTestSuite(ZonedDecimalFromHostTest.class);
		suite.addTestSuite(BinaryToHostTest.class);
		suite.addTestSuite(StringFromHostTest.class);
		suite.addTestSuite(PackedDecimalToHostTest.class);
		suite.addTestSuite(PackedDecimalFromHostTest.class);
		suite.addTestSuite(ZonedDecimalToHostTest.class);
		suite.addTestSuite(StringlToHostTest.class);
		suite.addTestSuite(BinaryFromHostTest.class);
		//$JUnit-END$
		return suite;
	}

}

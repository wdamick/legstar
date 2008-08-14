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
package com.legstar.coxb.gen;

import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.util.JaxbUtil;

import junit.framework.TestCase;

public class CoxbHelperTest extends TestCase {
	
	private CoxbHelper mCoxbHelper = new CoxbHelper();
	
	public void testGetBoundTypeName() throws Exception {
		com.legstar.test.coxb.lsfilead.ObjectFactory objectFactory
		= new com.legstar.test.coxb.lsfilead.ObjectFactory();

		CComplexReflectBinding binding = new CComplexReflectBinding(
				objectFactory,
				JaxbUtil.loadClass("com.legstar.test.coxb.alltypes.DfhcommareaType"));
		
		assertEquals("DfhcommareaType", mCoxbHelper.getBoundTypeName(binding));

	}

}

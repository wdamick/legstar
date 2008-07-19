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
package com.legstar.codegen;

import junit.framework.TestCase;

public class CodeGenHelperTest extends TestCase {
	
	public void testGetQualClassName() {
		CodeGenHelper helper = new CodeGenHelper();
		assertEquals("className", helper.getQualClassName(null, "className"));
		assertEquals("package.className", helper.getQualClassName("package", "className"));
	}

}

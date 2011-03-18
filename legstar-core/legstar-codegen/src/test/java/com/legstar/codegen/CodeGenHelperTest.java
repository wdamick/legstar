/*******************************************************************************
 * Copyright (c) 2011 LegSem.
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

/**
 * Generic test code.
 *
 */
public class CodeGenHelperTest extends TestCase {

    /**
     * Get fully qualified class name.
     */
    public void testGetQualClassName() {
        CodeGenHelper helper = new CodeGenHelper();
        assertEquals("className", helper.getQualClassName(null, "className"));
        assertEquals("package.className", helper.getQualClassName("package", "className"));
    }

}

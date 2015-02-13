/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.impl.reflect;

import com.legstar.test.coxb.RedmultiCases;

/**
 * Test Redmulti.
 *
 */
public class MarshalRedmultiTest extends AbstractTestMarshal {

    /**
     * Convert and check result for first alternative.
     */
    public void testRedmultiNormal() {
        convertAndCheck(
                RedmultiCases.getFactory(),
                RedmultiCases.getJavaObject(),
                RedmultiCases.getHostBytesHex());
    }

    /**
     * Convert and check result for second alternative.
     */
    public void testRedmultiError() {
        convertAndCheck(
                RedmultiCases.getFactory(),
                RedmultiCases.getJavaObjectError(),
                RedmultiCases.getHostBytesHexError());
    }
}

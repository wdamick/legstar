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
package com.legstar.coxb.impl.reflect;

import com.legstar.test.coxb.VararcomCases;

/**
 * Test Vararcom.
 *
 */
public class MarshalVararcomTest extends AbstractTestMarshal {

    /**
     * Case of empty java object.
     * Convert and check result.
     */
    public void testVararcomEmpty() {
        convertAndCheck(
                VararcomCases.getFactory(),
                VararcomCases.getJavaObjectEmpty(),
                VararcomCases.getHostBytesHexEmpty());
    }

    /**
     * Case of java object with some data.
     * Convert and check result.
     */
    public void testVararcomSome() {
        convertAndCheck(
                VararcomCases.getFactory(),
                VararcomCases.getJavaObjectSome(),
                VararcomCases.getHostBytesHexSome());
    }

    /**
     * Case of java object full of items.
     * Convert and check result.
     */
    public void testVararcomFull() {
        convertAndCheck(
                VararcomCases.getFactory(),
                VararcomCases.getJavaObjectFull(),
                VararcomCases.getHostBytesHexFull());
    }
}

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

import com.legstar.test.coxb.VararcomCases;
import com.legstar.test.coxb.vararcom.Dfhcommarea;

/**
 * Test VARARCOM.
 *
 */
public class UnmarshalVararcomTest extends AbstractTestUnmarshal {

    /**
     * Case of empty java object.
     * Unmarshal FIXARCOM.
     */
    public final void testVararcomEmpty() {
        Dfhcommarea dfhcommarea = (Dfhcommarea) convert(
                VararcomCases.getFactory(),
                VararcomCases.getHostBytesHexEmpty(),
                VararcomCases.getJavaObjectEmpty());
        VararcomCases.checkJavaObjectEmpty(dfhcommarea);
    }

    /**
     * Case of java object with some data.
     * Unmarshal FIXARCOM.
     */
    public final void testVararcomSome() {
        Dfhcommarea dfhcommarea = (Dfhcommarea) convert(
                VararcomCases.getFactory(),
                VararcomCases.getHostBytesHexSome(),
                VararcomCases.getJavaObjectSome());
        VararcomCases.checkJavaObjectSome(dfhcommarea);
    }

    /**
     * Case of java object full of items.
     * Unmarshal FIXARCOM.
     */
    public final void testVararcomFull() {
        Dfhcommarea dfhcommarea = (Dfhcommarea) convert(
                VararcomCases.getFactory(),
                VararcomCases.getHostBytesHexFull(),
                VararcomCases.getJavaObjectFull());
        VararcomCases.checkJavaObjectFull(dfhcommarea);
    }
}

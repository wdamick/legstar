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

import com.legstar.test.coxb.RedmultiCases;
import com.legstar.test.coxb.redmulti.Dfhcommarea;

/**
 * Test REDSIMPT.
 *
 */
public class UnmarshalRedmultiTest extends AbstractTestUnmarshal {
    /**
     * Unmarshal Redmulti first alternative.
     */
    public final void testRedmultiNormal() {
        Dfhcommarea dfhcommarea = (Dfhcommarea) convert(
                RedmultiCases.getFactory(),
                RedmultiCases.getHostBytesHex(),
                RedmultiCases.getJavaObject());
        RedmultiCases.checkJavaObject(dfhcommarea);
    }

    /**
     * Unmarshal Redmulti second alternative.
     */
    public final void testRedmultiError() {
        Dfhcommarea dfhcommarea = (Dfhcommarea) convert(
                RedmultiCases.getFactory(),
                RedmultiCases.getHostBytesHexError(),
                RedmultiCases.getJavaObjectError());
        RedmultiCases.checkJavaObjectError(dfhcommarea);
    }
}

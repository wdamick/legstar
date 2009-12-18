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

import com.legstar.test.coxb.ArrayssmCases;
import com.legstar.test.coxb.arrayssm.Dfhcommarea;

/**
 * Test ARRAYSSM.
 *
 */
public class UnmarshalArrayssmTest extends AbstractTestUnmarshal {
    /**
     * Unmarshal Arrayssm.
     */
    public void testArrayssm() {
        Dfhcommarea dfhcommarea = (Dfhcommarea) convert(
                ArrayssmCases.getFactory(),
                ArrayssmCases.getHostBytesHex(),
                ArrayssmCases.getJavaObject());
        ArrayssmCases.checkJavaObject(dfhcommarea);
    }

}

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

import com.legstar.test.coxb.DplarchtCases;
import com.legstar.test.coxb.dplarcht.Dfhcommarea;

/**
 * Test REDSIMPT.
 *
 */
public class UnmarshalDplarchtTest extends AbstractTestUnmarshal {
    /**
     * Unmarshal Dplarcht first alternative.
     */
    public void testDplarchtFilesChoice() {
        Dfhcommarea dfhcommarea = (Dfhcommarea) convert(
                DplarchtCases.getFactory(),
                DplarchtCases.getHostBytesHex1File(),
                DplarchtCases.getJavaObject());
        DplarchtCases.checkJavaObject1File(dfhcommarea);
    }

    /**
     * Unmarshal Dplarcht second alternative.
     */
    public void testDplarchtTransactionChoice() {
        Dfhcommarea dfhcommarea = (Dfhcommarea) convert(
                DplarchtCases.getFactory(),
                DplarchtCases.getHostBytesHex1Transaction(),
                DplarchtCases.getJavaObject());
        DplarchtCases.checkJavaObject1Transaction(dfhcommarea);
    }

    /**
     * Unmarshal Dplarcht third alternative.
     */
    public void testDplarchtProgramChoice() {
        Dfhcommarea dfhcommarea = (Dfhcommarea) convert(
                DplarchtCases.getFactory(),
                DplarchtCases.getHostBytesHex1Program(),
                DplarchtCases.getJavaObject());
        DplarchtCases.checkJavaObject1Program(dfhcommarea);
    }
}

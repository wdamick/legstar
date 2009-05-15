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

import com.legstar.test.coxb.FixarsimCases;
import com.legstar.test.coxb.fixarsim.Dfhcommarea;

/**
 * Test LSFILEAE.
 *
 */
public class UnmarshalFixarsimTest extends AbstractTestUnmarshal {

    /**
     * Unmarshal FIXARSIM.
     */
    public void testFixarsim() {
        Dfhcommarea dfhcommarea = (Dfhcommarea) convert(
                FixarsimCases.getFactory(),
                FixarsimCases.getHostBytesHex(),
                FixarsimCases.getJavaObject());
        FixarsimCases.checkJavaObject(dfhcommarea);
    }

}

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

import com.legstar.test.coxb.NumzonedCases;
import com.legstar.test.coxb.numzoned.Dfhcommarea;

/**
 * Test NUMZONED.
 *
 */
public class UnmarshalNumzonedTest extends AbstractTestUnmarshal {
    /**
     * Unmarshal Numzoned.
     */
    public void testNumzoned() {
        Dfhcommarea dfhcommarea = (Dfhcommarea) convert(
                NumzonedCases.getFactory(),
                NumzonedCases.getHostBytesHex(),
                NumzonedCases.getJavaObject());
        NumzonedCases.checkJavaObject(dfhcommarea);
    }

}

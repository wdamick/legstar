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

import com.legstar.test.coxb.RedsimptCases;
import com.legstar.test.coxb.redsimpt.Dfhcommarea;

/**
 * Test REDSIMPT.
 *
 */
public class UnmarshalRedsimptTest extends AbstractTestUnmarshal {
    /**
     * Unmarshal Redsimpt first alternative.
     */
    public void testRedsimptFirstAlternative() {
        Dfhcommarea dfhcommarea = (Dfhcommarea) convert(
                RedsimptCases.getFactory(),
                RedsimptCases.getHostBytesHex(),
                RedsimptCases.getJavaObject());
        RedsimptCases.checkJavaObject(dfhcommarea);
    }

    /**
     * Unmarshal Redsimpt second alternative.
     */
    public void testRedsimptSecondAlternative() {
        Dfhcommarea dfhcommarea = (Dfhcommarea) convert(
                RedsimptCases.getFactory(),
                RedsimptCases.getHostBytesHexSecondChoice(),
                RedsimptCases.getJavaObjectSecondChoice());
        RedsimptCases.checkJavaObjectSecondChoice(dfhcommarea);
    }
}

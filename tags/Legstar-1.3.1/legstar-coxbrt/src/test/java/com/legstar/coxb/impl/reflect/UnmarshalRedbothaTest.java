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

import com.legstar.test.coxb.RedbothaCases;
import com.legstar.test.coxb.redbotha.Dfhcommarea;

/**
 * Test REDBOTHA.
 *
 */
public class UnmarshalRedbothaTest extends AbstractTestUnmarshal {
    /**
     * Unmarshal Redbotha first alternative.
     */
    public void testRedbothaFirstAlternative() {
        Dfhcommarea dfhcommarea = (Dfhcommarea) convert(
                RedbothaCases.getFactory(),
                RedbothaCases.getHostBytesHex(),
                RedbothaCases.getJavaObject());
        RedbothaCases.checkJavaObject(dfhcommarea);
    }

    /**
     * Unmarshal Redbotha second alternative.
     */
    public void testRedbothaSecondAlternative() {
        Dfhcommarea dfhcommarea = (Dfhcommarea) convert(
                RedbothaCases.getFactory(),
                RedbothaCases.getHostBytesHexSecondChoice(),
                RedbothaCases.getJavaObjectSecondChoice());
        RedbothaCases.checkJavaObjectSecondChoice(dfhcommarea);
    }
}

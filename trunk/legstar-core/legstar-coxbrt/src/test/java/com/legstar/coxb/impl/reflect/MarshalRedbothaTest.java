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

import com.legstar.test.coxb.RedbothaCases;

/**
 * Test Redbotha.
 *
 */
public class MarshalRedbothaTest extends AbstractTestMarshal {

    /**
     * Convert and check result for first alternative.
     */
    public void testRedbothaFirstAlternative() {
        convertAndCheck(
                RedbothaCases.getFactory(),
                RedbothaCases.getJavaObject(),
                RedbothaCases.getHostBytesHex());
    }

    /**
     * Convert and check result for second alternative.
     */
    public void testRedbothaSecondAlternative() {
        convertAndCheck(
                RedbothaCases.getFactory(),
                RedbothaCases.getJavaObjectSecondChoice(),
                RedbothaCases.getHostBytesHexSecondChoice());
    }
}

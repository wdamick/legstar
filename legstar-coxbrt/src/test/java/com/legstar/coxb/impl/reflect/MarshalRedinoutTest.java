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

import com.legstar.test.coxb.RedinoutCases;

/**
 * Test Redinout.
 *
 */
public class MarshalRedinoutTest extends AbstractTestMarshal {

    /**
     * Convert and check result for first alternative.
     */
    public void testRedinoutFirstAlternative() {
        convertAndCheck(
                RedinoutCases.getFactory(),
                RedinoutCases.getJavaObject(),
                RedinoutCases.getHostBytesHex());
    }

    /**
     * Convert and check result for second alternative.
     */
    public void testRedinoutSecondAlternative() {
        convertAndCheck(
                RedinoutCases.getFactory(),
                RedinoutCases.getJavaObjectSecondChoice(),
                RedinoutCases.getHostBytesHexSecondChoice());
    }
}

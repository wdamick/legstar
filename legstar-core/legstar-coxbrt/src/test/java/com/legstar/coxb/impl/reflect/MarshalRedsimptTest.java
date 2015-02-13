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

/**
 * Test REDSIMPT.
 *
 */
public class MarshalRedsimptTest extends AbstractTestMarshal {

    /**
     * Convert and check result for first alternative.
     */
    public void testRedsimptFirstAlternative() {
        convertAndCheck(
                RedsimptCases.getFactory(),
                RedsimptCases.getJavaObject(),
                RedsimptCases.getHostBytesHex());
    }

    /**
     * Convert and check result for second alternative.
     */
    public void testRedsimptSecondAlternative() {
        convertAndCheck(
                RedsimptCases.getFactory(),
                RedsimptCases.getJavaObjectSecondChoice(),
                RedsimptCases.getHostBytesHexSecondChoice());
    }
}

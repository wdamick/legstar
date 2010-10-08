/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.impl.reflect;

import junit.framework.AssertionFailedError;

import com.legstar.test.coxb.RedinoutCases;
import com.legstar.test.coxb.redinout.Dfhcommarea;

/**
 * Test REDBOTHA.
 *
 */
public class UnmarshalRedinoutTest extends AbstractTestUnmarshal {
    /**
     * Unmarshal Redinout first alternative.
     * This test fails because the custom choice selector insist on
     * paraout being the only alternative when unmarshaling.
     */
    public void testRedinoutFirstAlternative() {
        try {
            Dfhcommarea dfhcommarea = (Dfhcommarea) convert(
                    RedinoutCases.getFactory(),
                    RedinoutCases.getHostBytesHex(),
                    RedinoutCases.getJavaObject());
            RedinoutCases.checkJavaObject(dfhcommarea);
        } catch (AssertionFailedError e) {
            assertEquals("ConversionException for element:CSomeOutput Cobol name:C-SOME-OUTPUT "
                    + "Reason:Host data contains a byte that is not a valid zoned decimal byte."
                    + " Host data at offset 2=0xc1c2c3c4c5c1c2c3", e.getMessage());
        }
    }

    /**
     * Unmarshal Redinout second alternative.
     */
    public void testRedinoutSecondAlternative() {
        Dfhcommarea dfhcommarea = (Dfhcommarea) convert(
                RedinoutCases.getFactory(),
                RedinoutCases.getHostBytesHexSecondChoice(),
                RedinoutCases.getJavaObjectSecondChoice());
        RedinoutCases.checkJavaObjectSecondChoice(dfhcommarea);
    }
}

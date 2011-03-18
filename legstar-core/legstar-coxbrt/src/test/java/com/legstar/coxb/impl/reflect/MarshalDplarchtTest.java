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

import junit.framework.AssertionFailedError;

import com.legstar.test.coxb.DplarchtCases;
import com.legstar.test.coxb.dplarcht.Dfhcommarea;
import com.legstar.test.coxb.dplarcht.LsRequest;

/**
 * Test Dplarcht.
 *
 */
public class MarshalDplarchtTest extends AbstractTestMarshal {

    /**
     * Create a situation where there is not enough data to determine
     * which alternative to use for the REDEFINE case.
     * Convert and check result.
     */
    public void testDplarchtNoAlternative() {
        Dfhcommarea dfhcommarea = new Dfhcommarea();
        dfhcommarea.setLsRequest(new LsRequest());
        
        try {
            convertAndCheck(
                    DplarchtCases.getFactory(),
                    dfhcommarea,
                    DplarchtCases.getHostBytesHex());
        } catch (AssertionFailedError e) {
            assertEquals("No alternative found for choice element LsAllItemsChoice", e.getMessage());
        }
    }

    /**
     * Convert and check result.
     */
    public void testDplarcht() {
        convertAndCheck(
                DplarchtCases.getFactory(),
                DplarchtCases.getJavaObject(),
                DplarchtCases.getHostBytesHex());
    }


    /**
     * Case of a program alternative.
     * Convert and check result.
     */
    public void testDplarcht1Program() {
        convertAndCheck(
                DplarchtCases.getFactory(),
                DplarchtCases.getJavaObject1Program(),
                DplarchtCases.getHostBytesHex1Program());
    }
}

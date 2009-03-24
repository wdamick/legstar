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
package com.legstar.mock.client;

import java.io.IOException;
import java.util.List;

import com.legstar.test.coxb.lsfileae.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Test the MockFILEAE class.
 *
 */
public class MockFILEAETest extends TestCase {
    
    /**
     * Check that text file is loaded properly.
     */
    public void testLoadTextFile() {
        try {
            MockFILEA mockFILEAE = new MockFILEA();
            List < Dfhcommarea > list = mockFILEAE.getCustomers("S*", 100);
            assertEquals(5, list.size());
            list = mockFILEAE.getCustomers("*", 100);
            assertEquals(43, list.size());
        } catch (IOException e) {
            fail(e.getMessage());
        }
    }

}

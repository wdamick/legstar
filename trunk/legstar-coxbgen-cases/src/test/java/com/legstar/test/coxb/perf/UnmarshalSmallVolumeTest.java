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
package com.legstar.test.coxb.perf;

import com.legstar.coxb.host.HostException;

/**
 * Test unmarshaling a small size workload. 
 *
 */
public class UnmarshalSmallVolumeTest extends AbstractUnmarshalVolume {


    /** DPLARCHT header for 0 items.*/
    private static final String DPLARCHT_BYTES_HEADER  =
          "0000"
        + "5c404040"
        + "c340404040404040"
        + "000000001f"
        + "0000"
        + "00000000";
    
    /**
     * Create the test case.
     */
    public UnmarshalSmallVolumeTest() {
        super(100, DPLARCHT_BYTES_HEADER, 0, 100L);
    }

    /**
     * Run the test case.
     * @throws HostException if test fails
     */
    public void testUnmarshal() throws HostException {
        unmarshal();
    }
}


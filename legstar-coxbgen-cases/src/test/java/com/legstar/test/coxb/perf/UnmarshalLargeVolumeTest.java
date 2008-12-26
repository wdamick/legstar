/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.test.coxb.perf;

import com.legstar.coxb.host.HostException;

/**
 * Test unmarshaling a large workload. 
 *
 */
public class UnmarshalLargeVolumeTest extends AbstractUnmarshalVolume {


    /** DPLARCHT header for 500 items.*/
    private static final String DPLARCHT_BYTES_HEADER  =
          "0000"
        + "5c404040"
        + "c340404040404040"
        + "000000001f"
        + "0000"
        + "000001f4";
    
    /**
     * Create the test case.
     */
    public UnmarshalLargeVolumeTest() {
        super(100, DPLARCHT_BYTES_HEADER, 500, 1000L);
    }

    /**
     * Run the test case.
     * @throws HostException if test fails
     */
    public void testUnmarshal() throws HostException {
        unmarshal();
    }
}

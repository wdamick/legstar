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

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.impl.visitor.CobolUnmarshalVisitor;
import com.legstar.test.coxb.dplarcht.ObjectFactory;
import com.legstar.test.coxb.dplarcht.Dfhcommarea;
import com.legstar.test.coxb.dplarcht.bind.DfhcommareaBinding;

import junit.framework.TestCase;

/**
 * Generic load test for unmarshaling. Based on DPLARCHT which has a variable size
 * workload and a mix of member types. 
 *
 */
public class AbstractUnmarshalVolume extends TestCase {

    /** Number of iterations. */
    private int mIterations;
    
    /** Hexadecimal representation of the DPLARCHY header. */
    private String mHostBytesHeaderHex;

    /** Maximum allowed duration in millisecs. */
    private long mMaxDurationMillis;
    
    /** Hex byte representation of a DPLARCHT item.*/
    private static final String DPLARCHT_ITEM_BYTES  =
        "c6c9d3c5f0404040"
      + "a38889a24b89a24b868993854bf0404040404040404040404040404040404040404040404040404040404040"
      + "c5d5c1c2d3c5c44040404040";

    /** How many items in the dplarcht variable size array. */
    private int mItemsCount;
    
    /**
     * Construct the test case.
     * @param iterations number of iterations
     * @param hostBytesHeaderHex Hexadecimal representation of the DPLARCHY header
     * @param itemsCount how many items in the dplarcht variable size array
     * @param maxDurationMillis the maximum duration allowed
     */
    public AbstractUnmarshalVolume(
            final int iterations,
            final String hostBytesHeaderHex,
            final int itemsCount,
            final long maxDurationMillis) {
        mIterations = iterations;
        mHostBytesHeaderHex = hostBytesHeaderHex;
        mMaxDurationMillis = maxDurationMillis;
        mItemsCount = itemsCount;
    }

    /**
     * Unmarshal a host data byte array to a java data object.
     * @throws HostException if unmarshaling fails.
     */
    public void unmarshal() throws HostException {
        StringBuffer sb = new StringBuffer(mHostBytesHeaderHex);
        for (int i = 0; i < mItemsCount; i++) {
            sb.append(DPLARCHT_ITEM_BYTES);
        }
        
        byte[] hostBytes = HostData.toByteArray(sb.toString());
        CobolSimpleConverters cc = new CobolSimpleConverters(new CobolContext());
        CobolUnmarshalVisitor mv = new CobolUnmarshalVisitor(hostBytes, 0, cc);
        ObjectFactory objectFactory = new ObjectFactory();

        // Perform mashaling a number of times
        long start = System.currentTimeMillis();
        for (int i = 0; i < mIterations; i++) {
            // Create and bind an instance of an object (JAXB annotated)
            Dfhcommarea dfhcommarea = objectFactory.createDfhcommarea();
            DfhcommareaBinding ccem = new DfhcommareaBinding(dfhcommarea);
            
            // Traverse the object structure, visiting each node with the visitor
            mv.setOffset(0);
            ccem.accept(mv);
        }
        long stop = System.currentTimeMillis();
        assertEquals((sb.length() / 2), mv.getOffset());
        assertTrue((stop - start) < mMaxDurationMillis);
    }
}

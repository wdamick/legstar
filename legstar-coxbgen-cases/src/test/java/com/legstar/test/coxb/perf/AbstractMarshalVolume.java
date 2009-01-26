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

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.impl.visitor.CobolMarshalVisitor;
import com.legstar.test.coxb.dplarcht.ObjectFactory;
import com.legstar.test.coxb.dplarcht.Dfhcommarea;
import com.legstar.test.coxb.dplarcht.LsRequest;
import com.legstar.test.coxb.dplarcht.LsSearchCriteria;
import com.legstar.test.coxb.dplarcht.LsReply;
import com.legstar.test.coxb.dplarcht.LsReplyData;
import com.legstar.test.coxb.dplarcht.LsItemsArray;
import com.legstar.test.coxb.dplarcht.LsFilesData;
import com.legstar.test.coxb.dplarcht.bind.DfhcommareaBinding;

import junit.framework.TestCase;

/**
 * Generic load test for marshaling. Based on DPLARCHT which has a variable size
 * workload and a mix of member types. 
 *
 */
public abstract class AbstractMarshalVolume extends TestCase {

    /** Number of iterations. */
    private int mIterations;
    
    /** How many items in the dplarcht variable size array. */
    private int mItemsCount;

    /** Host data byte size. */
    private int mHostByteSize;
    
    /** Maximum allowed duration in millisecs. */
    private long mMaxDurationMillis;
    
    /**
     * Construct the test case.
     * @param iterations number of iterations
     * @param itemsCount how many items in the dplarcht variable size array
     * @param hostByteSize host byte size
     * @param maxDurationMillis the maximum duration allowed
     */
    public AbstractMarshalVolume(
            final int iterations,
            final int itemsCount,
            final int hostByteSize,
            final long maxDurationMillis) {
        mIterations = iterations;
        mItemsCount = itemsCount;
        mHostByteSize = hostByteSize;
        mMaxDurationMillis = maxDurationMillis;
    }

    /**
     * Marshal a java data object into a host data byte array.
     * @throws HostException if marshaling fails.
     */
    public void marshal() throws HostException {
        byte[] hostBytes = new byte[mHostByteSize];

        CobolSimpleConverters cc = new CobolSimpleConverters(new CobolContext());
        CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0, cc);
        ObjectFactory objectFactory = new ObjectFactory();
        Dfhcommarea dfhcommarea = objectFactory.createDfhcommarea();

        LsRequest lsRequest = objectFactory.createLsRequest();
        dfhcommarea.setLsRequest(lsRequest);
        lsRequest.setLsRequestType(0); // request files
        lsRequest.setLsAllItems("*");  // no limit to number of items
        LsSearchCriteria lsSearchCriteria = objectFactory.createLsSearchCriteria();
        lsSearchCriteria.setLsStartwith("C");
        lsSearchCriteria.setLsStartwithLen(1);
        lsRequest.setLsSearchCriteria(lsSearchCriteria);

        LsReply lsReply = objectFactory.createLsReply();
        dfhcommarea.setLsReply(lsReply);
        LsReplyData lsReplyData = objectFactory.createLsReplyData();
        lsReply.setLsReplyData(lsReplyData);
        lsReplyData.setLsItemsCount(mItemsCount);

        for (int i = 0; i < mItemsCount; i++) {
            LsItemsArray ia = objectFactory.createLsItemsArray();
            LsFilesData dt = objectFactory.createLsFilesData();
            dt.setLsFileName("FILE" + (new Integer(i)).toString());
            dt.setLsFileDsname("this.is.file." + (new Integer(i)).toString());
            dt.setLsFileEnablestatus("ENABLED");
            ia.setLsFilesData(dt);
            lsReplyData.getLsItemsArray().add(ia);
        }

        // Perform mashaling a number of times
        long start = System.currentTimeMillis();
        for (int i = 0; i < mIterations; i++) {
            /* Bind java data object */
            DfhcommareaBinding ccem = new DfhcommareaBinding(dfhcommarea);
            /* Traverse the object structure, visiting each node with the visitor */
            mv.setOffset(0);
            ccem.accept(mv);
        }
        long stop = System.currentTimeMillis();
        assertEquals(mHostByteSize, mv.getOffset());
        assertTrue((stop - start) < mMaxDurationMillis);
    }
}

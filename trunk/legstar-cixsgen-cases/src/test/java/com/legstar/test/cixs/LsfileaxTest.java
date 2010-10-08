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
package com.legstar.test.cixs;

import com.legstar.test.cixs.lsfileax.LsfileacFault;
import com.legstar.test.cixs.lsfileax.LsfileacRequestHolder;
import com.legstar.test.cixs.lsfileax.LsfileacResponseHolder;
import com.legstar.test.cixs.lsfileax.LsfileaeFault;
import com.legstar.test.cixs.lsfileax.LsfileaxHostHeader;
import com.legstar.test.cixs.lsfileax.LsfileaxImpl;
import com.legstar.test.coxb.LsfileacCases;
import com.legstar.test.coxb.LsfileaeCases;
import com.legstar.test.coxb.lsfileac.QueryData;
import com.legstar.test.coxb.lsfileac.QueryLimit;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Test LSFILEAX adapter.
 *
 */
public class LsfileaxTest extends TestCase {
    
    /**
     * Direct host invoke.
     * @throws LsfileaeFault if test fails
     */
    public void testLsfileaeRequest100() throws LsfileaeFault {
        LsfileaxImpl port = new LsfileaxImpl();
        Dfhcommarea request = LsfileaeCases.getJavaObjectRequest100();
        Dfhcommarea reply = port.lsfileae(request, null);
        LsfileaeCases.checkJavaObjectReply100(reply);
    }

    /**
     * Direct host invoke.
     * @throws LsfileacFault if test fails
     */
    public void testLsfileacRequestSStar() throws LsfileacFault {
        LsfileaxImpl port = new LsfileaxImpl();
        QueryData queryData = LsfileacCases.getJavaObjectQueryData();
        QueryLimit queryLimit = LsfileacCases.getJavaObjectQueryLimit();
        LsfileacRequestHolder request = new LsfileacRequestHolder();
        request.setQueryData(queryData);
        request.setQueryLimit(queryLimit);
        LsfileaxHostHeader header = new LsfileaxHostHeader();
        header.setHostEndPoint("CICSTS31");
        LsfileacResponseHolder reply = port.lsfileac(request, header);
        LsfileacCases.checkJavaObjectReplyData(reply.getReplyData());
        LsfileacCases.checkJavaObjectReplyStatus(reply.getReplyStatus());
    }
}

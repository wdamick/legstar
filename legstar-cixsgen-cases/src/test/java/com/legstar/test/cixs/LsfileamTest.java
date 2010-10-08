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

import com.legstar.test.cixs.lsfileam.LsfileamFault;
import com.legstar.test.cixs.lsfileam.LsfileamHostHeader;
import com.legstar.test.cixs.lsfileam.LsfileamImpl;
import com.legstar.test.cixs.lsfileam.LsfileamRequestHolder;
import com.legstar.test.cixs.lsfileam.LsfileamResponseHolder;
import com.legstar.test.coxb.LsfileacCases;
import com.legstar.test.coxb.lsfileac.QueryData;
import com.legstar.test.coxb.lsfileac.QueryLimit;

import junit.framework.TestCase;

/**
 * Test LSFILEAM adapter.
 * 
 */
public class LsfileamTest extends TestCase {

    /**
     * Direct host invoke.
     * 
     * @throws LsfileamFault if test fails
     */
    public void testLsfileamRequest() throws LsfileamFault {
        LsfileamImpl port = new LsfileamImpl();
        QueryData queryData = LsfileacCases.getJavaObjectQueryData();
        QueryLimit queryLimit = LsfileacCases.getJavaObjectQueryLimit();
        LsfileamRequestHolder request = new LsfileamRequestHolder();
        request.setQueryData(queryData);
        request.setQueryLimit(queryLimit);
        LsfileamHostHeader header = new LsfileamHostHeader();
        header.setHostEndPoint("CICSTS31");
        LsfileamResponseHolder reply = port.lsfileam(request, header);
        LsfileacCases.checkJavaObjectReplyData(reply.getReplyData());
        LsfileacCases.checkJavaObjectReplyStatus(reply.getReplyStatus());
    }
}

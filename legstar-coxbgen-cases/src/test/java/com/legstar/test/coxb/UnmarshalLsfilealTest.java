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
package com.legstar.test.coxb;

import com.legstar.coxb.host.HostData;
import com.legstar.coxb.transform.HostTransformStatus;
import com.legstar.test.coxb.lsfileal.ReplyData;
import com.legstar.test.coxb.lsfileal.bind.ReplyDataHostToJavaTransformer;

import junit.framework.TestCase;

/**
 * Unmarshal lsfileal.
 * 
 */
public class UnmarshalLsfilealTest extends TestCase {

    /**
     * Unmarshal host data and test java data object result.
     * 
     * @throws Exception if marshaling fails
     */
    public void testLsfileal() throws Exception {

        String hexString = LsfilealCases.getHostBytesHex();
        byte[] hostBytes = HostData.toByteArray(hexString);
        ReplyData replyData = (ReplyData) Util.unmarshal(hostBytes, "lsfileal",
                "ReplyData");
        LsfilealCases.checkJavaObject(replyData);
    }

    /**
     * Transform host data and test java data object result.
     * 
     * @throws Exception if transforming fails
     */
    public void testHostToJavaTransformer() throws Exception {

        HostTransformStatus status = new HostTransformStatus();
        ReplyDataHostToJavaTransformer transformer = new ReplyDataHostToJavaTransformer();
        ReplyData replyData = transformer.transform(
                HostData.toByteArray(LsfilealCases.getHostBytesHex()), status);
        LsfilealCases.checkJavaObject(replyData);
        assertEquals(301, status.getHostBytesProcessed());
    }

    /**
     * Unmarshal host data and test java data object result.
     * Error case.
     * 
     * @throws Exception if marshaling fails
     */
    public void testLsfilealWithError() throws Exception {

        String hexString = LsfilealCases.getHostBytesHexError();
        byte[] hostBytes = HostData.toByteArray(hexString);
        ReplyData replyData = (ReplyData) Util.unmarshal(hostBytes, "lsfileal",
                "ReplyData");
        LsfilealCases.checkJavaObjectHexError(replyData);
    }

    /**
     * Transform host data and test java data object result.
     * 
     * @throws Exception if transforming fails
     */
    public void testHostToJavaTransformerWithError() throws Exception {

        ReplyDataHostToJavaTransformer transformer = new ReplyDataHostToJavaTransformer();
        ReplyData replyData = transformer.transform(
                HostData.toByteArray(LsfilealCases.getHostBytesHexError()));
        LsfilealCases.checkJavaObjectHexError(replyData);
    }
}

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
package com.legstar.test.coxb;



import com.legstar.coxb.host.HostData;
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
     * @throws Exception if marshaling fails
     */
    public void testLsfileal() throws Exception {

        String hexString = LsfilealCases.getHostBytesHex();
        byte[] hostBytes = HostData.toByteArray(hexString);
        ReplyData replyData = (ReplyData) Util.unmarshal(hostBytes, "lsfileal", "ReplyData");
        LsfilealCases.checkJavaObject(replyData);
    }

    /**
     * Transform host data and test java data object result.
     * @throws Exception if transforming fails
     */
    public void testHostToJavaTransformer() throws Exception {

        ReplyDataHostToJavaTransformer transformer = new ReplyDataHostToJavaTransformer();
        ReplyData replyData = transformer.transform(
                HostData.toByteArray(LsfilealCases.getHostBytesHex()));
        LsfilealCases.checkJavaObject(replyData);
    }

    /**
     * Unmarshal host data and test java data object result.
     * Error case.
     * @throws Exception if marshaling fails
     */
    public void testLsfilealWithError() throws Exception {

        String hexString = LsfilealCases.getHostBytesHexError();
        byte[] hostBytes = HostData.toByteArray(hexString);
        ReplyData replyData = (ReplyData) Util.unmarshal(hostBytes, "lsfileal", "ReplyData");
        LsfilealCases.checkJavaObjectHexError(replyData);
    }
    /**
     * Transform host data and test java data object result.
     * @throws Exception if transforming fails
     */
    public void testHostToJavaTransformerWithError() throws Exception {

        ReplyDataHostToJavaTransformer transformer = new ReplyDataHostToJavaTransformer();
        ReplyData replyData = transformer.transform(
                HostData.toByteArray(LsfilealCases.getHostBytesHexError()));
        LsfilealCases.checkJavaObjectHexError(replyData);
    }
}
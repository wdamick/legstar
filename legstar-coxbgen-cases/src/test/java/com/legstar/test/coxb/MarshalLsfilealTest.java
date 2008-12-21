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

import com.legstar.test.coxb.lsfileal.ReplyData;

import junit.framework.TestCase;

/**
 * Marshal lsfileal.
 *
 */
public class MarshalLsfilealTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "lsfileal";

    /**
     * Marshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testLsfileal() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        ReplyData replyData = LsfilealCases.getJavaObject();
        assertEquals(LsfilealCases.getHostBytesHex(),
                Util.marshal(SCHEMA_NAME, "ReplyData", replyData, 301));
    }

    /**
     * Marshal host data and test java data object result.
     * Error case.
     * @throws Exception if marshaling fails
     */
    public void testLsfilealError() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        ReplyData replyData = LsfilealCases.getJavaObjectError();
        assertEquals(LsfilealCases.getHostBytesHexError(),
                Util.marshal(SCHEMA_NAME, "ReplyData", replyData, 143));
    }
}

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
package com.legstar.test.coxb;

import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.lsfileal.bind.ReplyDataJavaToHostTransformer;
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
     * Marshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testLsfileal() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        ReplyData replyData = LsfilealCases.getJavaObject();
        assertEquals(LsfilealCases.getHostBytesHex(),
                Util.marshal(SCHEMA_NAME, "ReplyData", replyData, 301));
    }

    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformer() throws Exception {

        ReplyDataJavaToHostTransformer transformer = new ReplyDataJavaToHostTransformer();
        assertEquals(LsfilealCases.getHostBytesHex(),
                HostData.toHexString(transformer.transform(LsfilealCases.getJavaObject())));
    }
    /**
     * Marshal java data object and test host data result.
     * Error case.
     * @throws Exception if marshaling fails
     */
    public void testLsfilealError() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        ReplyData replyData = LsfilealCases.getJavaObjectError();
        assertEquals(LsfilealCases.getHostBytesHexError(),
                Util.marshal(SCHEMA_NAME, "ReplyData", replyData, 143));
    }
    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformerError() throws Exception {

        ReplyDataJavaToHostTransformer transformer = new ReplyDataJavaToHostTransformer();
        assertEquals(LsfilealCases.getHostBytesHexError(),
                HostData.toHexString(transformer.transform(LsfilealCases.getJavaObjectError())));
    }
}

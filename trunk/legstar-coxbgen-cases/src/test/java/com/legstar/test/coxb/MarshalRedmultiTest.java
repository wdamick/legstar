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
import com.legstar.test.coxb.redmulti.bind.DfhcommareaJavaToHostTransformer;
import com.legstar.test.coxb.redmulti.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Marshal redmulti.
 *
 */
public class MarshalRedmultiTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "redmulti";

    /**
     * Marshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testRedmultiNormal() throws Exception {
        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = RedmultiCases.getJavaObject();
        assertEquals(RedmultiCases.getHostBytesHex(),
                Util.marshal(SCHEMA_NAME, dfhcommarea, 206));
    }

    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformerNormal() throws Exception {

        DfhcommareaJavaToHostTransformer transformer = new DfhcommareaJavaToHostTransformer();
        assertEquals(RedmultiCases.getHostBytesHex(),
                HostData.toHexString(transformer.transform(RedmultiCases.getJavaObject())));
    }
    /**
     * Marshal java data object and test host data result.
     * Alternative choice.
     * @throws Exception if marshaling fails
     */
    public void testRedmultiError() throws Exception {
        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = RedmultiCases.getJavaObjectError();
        assertEquals(RedmultiCases.getHostBytesHexError(),
                Util.marshal(SCHEMA_NAME, dfhcommarea, 206));
    }

    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformerError() throws Exception {

        DfhcommareaJavaToHostTransformer transformer = new DfhcommareaJavaToHostTransformer();
        assertEquals(RedmultiCases.getHostBytesHexError(),
                HostData.toHexString(transformer.transform(RedmultiCases.getJavaObjectError())));
    }
}

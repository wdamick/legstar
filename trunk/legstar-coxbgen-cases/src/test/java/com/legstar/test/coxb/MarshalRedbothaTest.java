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
import com.legstar.test.coxb.redbotha.bind.DfhcommareaJavaToHostTransformer;
import com.legstar.test.coxb.redbotha.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Marshal redbotha.
 *
 */
public class MarshalRedbothaTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "redbotha";

    /**
     * Marshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testRedbotha() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = RedbothaCases.getJavaObject();
        assertEquals(RedbothaCases.getHostBytesHex(),
                Util.marshal(SCHEMA_NAME, dfhcommarea, 2));
    }

    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformer() throws Exception {

        DfhcommareaJavaToHostTransformer transformer = new DfhcommareaJavaToHostTransformer();
        assertEquals(RedbothaCases.getHostBytesHex(),
                HostData.toHexString(transformer.transform(RedbothaCases.getJavaObject())));
    }
    /**
     * Marshal java data object and test host data result.
     * Alternative choice.
     * @throws Exception if marshaling fails
     */
    public void testRedbothaSecondChoice() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = RedbothaCases.getJavaObjectSecondChoice();
        assertEquals(RedbothaCases.getHostBytesHexSecondChoice(),
                Util.marshal(SCHEMA_NAME, dfhcommarea, 2));
    }
    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformerSecondChoice() throws Exception {

        DfhcommareaJavaToHostTransformer transformer = new DfhcommareaJavaToHostTransformer();
        assertEquals(RedbothaCases.getHostBytesHexSecondChoice(),
                HostData.toHexString(transformer.transform(RedbothaCases.getJavaObjectSecondChoice())));
    }
}

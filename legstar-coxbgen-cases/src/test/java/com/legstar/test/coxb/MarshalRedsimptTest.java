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
import com.legstar.test.coxb.redsimpt.bind.DfhcommareaJavaToHostTransformer;
import com.legstar.test.coxb.redsimpt.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Marshal redsimpt.
 *
 */
public class MarshalRedsimptTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "redsimpt";

    /**
     * Marshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testRedsimpt() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = RedsimptCases.getJavaObject();
        assertEquals(RedsimptCases.getHostBytesHex(),
                Util.marshal(SCHEMA_NAME, dfhcommarea, 18));
    }

    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformer() throws Exception {

        DfhcommareaJavaToHostTransformer transformer = new DfhcommareaJavaToHostTransformer();
        assertEquals(RedsimptCases.getHostBytesHex(),
                HostData.toHexString(transformer.transform(RedsimptCases.getJavaObject())));
    }
    /**
     * Marshal java data object and test host data result.
     * Alternative choice.
     * @throws Exception if marshaling fails
     */
    public void testRedsimptSecondChoice() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = RedsimptCases.getJavaObjectSecondChoice();
        assertEquals(RedsimptCases.getHostBytesHexSecondChoice(),
                Util.marshal(SCHEMA_NAME, dfhcommarea, 18));
    }
    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformerSecondChoice() throws Exception {

        DfhcommareaJavaToHostTransformer transformer = new DfhcommareaJavaToHostTransformer();
        assertEquals(RedsimptCases.getHostBytesHexSecondChoice(),
                HostData.toHexString(transformer.transform(RedsimptCases.getJavaObjectSecondChoice())));
    }
}

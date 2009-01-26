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
import com.legstar.test.coxb.numzoned.bind.DfhcommareaJavaToHostTransformer;
import com.legstar.test.coxb.numzoned.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Marshal numzoned.
 *
 */
public class MarshalNumzonedTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "numzoned";

    /**
     * Marshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testLsfileae() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = NumzonedCases.getJavaObject();
        assertEquals(NumzonedCases.getHostBytesHex(),
                Util.marshal(SCHEMA_NAME, dfhcommarea, 13));
    }
    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformer() throws Exception {

        DfhcommareaJavaToHostTransformer transformer = new DfhcommareaJavaToHostTransformer();
        assertEquals(NumzonedCases.getHostBytesHex(),
                HostData.toHexString(transformer.transform(NumzonedCases.getJavaObject())));
    }
}

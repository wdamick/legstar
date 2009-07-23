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
import com.legstar.test.coxb.fixarcom.bind.DfhcommareaJavaToHostTransformer;
import com.legstar.test.coxb.fixarcom.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Marshal fixarcom.
 *
 */
public class MarshalFixarcomTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "fixarcom";

    /**
     * Marshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testFixarcom() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = FixarcomCases.getJavaObject();
        assertEquals(FixarcomCases.getHostBytesHex(),
                Util.marshal(SCHEMA_NAME, dfhcommarea, 49));
    }

    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformer() throws Exception {

        DfhcommareaJavaToHostTransformer transformer = new DfhcommareaJavaToHostTransformer();
        assertEquals(FixarcomCases.getHostBytesHex(),
                HostData.toHexString(transformer.transform(FixarcomCases.getJavaObject())));
    }
}

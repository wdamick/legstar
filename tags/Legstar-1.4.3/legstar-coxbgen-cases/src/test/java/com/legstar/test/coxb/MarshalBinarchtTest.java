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
import com.legstar.test.coxb.binarcht.bind.DfhcommareaJavaToHostTransformer;
import com.legstar.test.coxb.binarcht.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Marshal binarcht.
 *
 */
public class MarshalBinarchtTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "binarcht";

    /**
     * Marshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testBinarcht() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = BinarchtCases.getJavaObject();

        assertEquals(BinarchtCases.getHostBytesHex(),
                Util.marshal(SCHEMA_NAME, dfhcommarea, 56));
    }
    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformer() throws Exception {

        DfhcommareaJavaToHostTransformer transformer = new DfhcommareaJavaToHostTransformer();
        assertEquals(BinarchtCases.getHostBytesHex(),
                HostData.toHexString(transformer.transform(BinarchtCases.getJavaObject())));
    }
}

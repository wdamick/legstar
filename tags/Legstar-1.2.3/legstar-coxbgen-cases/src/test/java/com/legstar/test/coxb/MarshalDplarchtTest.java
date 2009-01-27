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
import com.legstar.test.coxb.dplarcht.bind.DfhcommareaJavaToHostTransformer;
import com.legstar.test.coxb.dplarcht.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Marshal dplarcht.
 *
 */
public class MarshalDplarchtTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "dplarcht";

    /**
     * Marshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testDplarcht() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = DplarchtCases.getJavaObject();
        assertEquals(DplarchtCases.getHostBytesHex(),
                Util.marshal(SCHEMA_NAME, dfhcommarea, DplarchtCases.getHostBytesHex().length() / 2));
    }
    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformer() throws Exception {

        DfhcommareaJavaToHostTransformer transformer = new DfhcommareaJavaToHostTransformer();
        assertEquals(DplarchtCases.getHostBytesHex1Program().toLowerCase(),
                HostData.toHexString(transformer.transform(DplarchtCases.getJavaObject1Program())));
    }
}

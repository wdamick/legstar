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
import com.legstar.test.coxb.valuemix.bind.DfhcommareaJavaToHostTransformer;
import com.legstar.test.coxb.valuemix.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Marshal valuemix.
 *
 */
public class MarshalValuemixTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "valuemix";
    
    /**
     * Marshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testValuemix() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = ValuemixCases.getJavaObject();
        assertEquals(ValuemixCases.getHostBytesHex(),
                Util.marshal(SCHEMA_NAME, dfhcommarea, ValuemixCases.getHostBytesHex().length() / 2));
    }

    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformer() throws Exception {

        DfhcommareaJavaToHostTransformer transformer = new DfhcommareaJavaToHostTransformer();
        assertEquals(ValuemixCases.getHostBytesHex(),
                HostData.toHexString(transformer.transform(ValuemixCases.getJavaObject())));
    }
}

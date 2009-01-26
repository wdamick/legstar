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
import com.legstar.test.coxb.redopera.bind.DfhcommareaJavaToHostTransformer;
import com.legstar.test.coxb.redopera.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Marshal redopera.
 *
 */
public class MarshalRedoperaTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "redopera";

    /**
     * Marshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testRedoperaStringMethod() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = RedoperaCases.getJavaObject();
        assertEquals(RedoperaCases.getHostBytesHex(),
                Util.marshal(SCHEMA_NAME, dfhcommarea, 218));
    }

    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformerStringMethod() throws Exception {

        DfhcommareaJavaToHostTransformer transformer = new DfhcommareaJavaToHostTransformer();
        assertEquals(RedoperaCases.getHostBytesHex(),
                HostData.toHexString(transformer.transform(RedoperaCases.getJavaObject())));
    }
    /**
     * Marshal java data object and test host data result.
     * Alternative choice.
     * @throws Exception if marshaling fails
     */
    public void testRedoperaIntMethod() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = RedoperaCases.getJavaObjectIntMethod();
        assertEquals(RedoperaCases.getHostBytesHexIntMethod(),
                Util.marshal(SCHEMA_NAME, dfhcommarea, 218));
    }
    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformerIntMethod() throws Exception {

        DfhcommareaJavaToHostTransformer transformer = new DfhcommareaJavaToHostTransformer();
        assertEquals(RedoperaCases.getHostBytesHexIntMethod(),
                HostData.toHexString(transformer.transform(RedoperaCases.getJavaObjectIntMethod())));
    }
}

/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
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

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
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.test.coxb.lsfileae.ComPersonal;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;
import com.legstar.test.coxb.lsfileae.ObjectFactory;
import com.legstar.test.coxb.lsfileae.bind.DfhcommareaJavaToHostTransformer;

import junit.framework.TestCase;

/**
 * Marshal lsfileae.
 *
 */
public class MarshalLsfileaeTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "lsfileae";

    /**
     * Marshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testLsfileae() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = LsfileaeCases.getJavaObject();
        assertEquals(LsfileaeCases.getHostBytesHex(),
                Util.marshal(SCHEMA_NAME, dfhcommarea, 79));
    }
    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformer() throws Exception {

        DfhcommareaJavaToHostTransformer transformer = new DfhcommareaJavaToHostTransformer();
        assertEquals(LsfileaeCases.getHostBytesHex(),
                HostData.toHexString(transformer.transform(LsfileaeCases.getJavaObject())));
    }

    /**
     * Test the sample code shown in documentation.
     * @throws HostTransformException if transforming fails
     */
    public void testHostToJavaTransformerDoc() throws HostTransformException {

        assertEquals(LsfileaeCases.getHostBytesHex(),
                HostData.toHexString(javaToHostTransform()));
    }

    /**
     * Creates a java data object and returns the host data result.
     * @return a byte array holding the mainframe payload
     * @throws HostTransformException if transforming fails
     */
    public byte[] javaToHostTransform() throws HostTransformException {
        ObjectFactory of = new ObjectFactory();
        Dfhcommarea dfhcommarea = of.createDfhcommarea();
        dfhcommarea.setComNumber(100L);
        ComPersonal comPersonal = of.createComPersonal();
        comPersonal.setComName("TOTO");
        comPersonal.setComAddress("LABAS STREET");
        comPersonal.setComPhone("88993314");
        dfhcommarea.setComPersonal(comPersonal);
        dfhcommarea.setComDate("100458");
        dfhcommarea.setComAmount("00100.35");
        dfhcommarea.setComComment("A VOIR");
        DfhcommareaJavaToHostTransformer transformer = new DfhcommareaJavaToHostTransformer();
        return transformer.transform(dfhcommarea);
    }
}

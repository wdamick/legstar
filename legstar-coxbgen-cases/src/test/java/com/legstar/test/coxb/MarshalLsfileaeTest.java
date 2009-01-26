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
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.test.coxb.lsfileae.ComPersonal;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;
import com.legstar.test.coxb.lsfileae.ObjectFactory;
import com.legstar.test.coxb.lsfileae.bind.DfhcommareaJavaToHostTransformer;
import com.legstar.test.coxb.lsfileae.bind.DfhcommareaTransformers;

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
        DfhcommareaTransformers transformers = new DfhcommareaTransformers();
        return transformers.toHost(dfhcommarea);
    }
}

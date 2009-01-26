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
import com.legstar.test.coxb.vararcom.bind.DfhcommareaJavaToHostTransformer;
import com.legstar.test.coxb.vararcom.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Marshal vararcom.
 *
 */
public class MarshalVararcomTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "vararcom";

    /**
     * Marshal java data object and test host data result.
     * Empty case.
     * @throws Exception if marshaling fails
     */
    public void testVararcomEmpty() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = VararcomCases.getJavaObjectEmpty();
        assertEquals(VararcomCases.getHostBytesHexEmpty(),
                Util.marshal(SCHEMA_NAME, dfhcommarea, 2));
    }

    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformerEmpty() throws Exception {

        DfhcommareaJavaToHostTransformer transformer = new DfhcommareaJavaToHostTransformer();
        assertEquals(VararcomCases.getHostBytesHexEmpty(),
                HostData.toHexString(transformer.transform(VararcomCases.getJavaObjectEmpty())));
    }

    /**
     * Marshal java data object and test host data result.
     * Partially filled case.
     * @throws Exception if marshaling fails
     */
    public void testVararcomSome() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = VararcomCases.getJavaObjectSome();
        assertEquals(VararcomCases.getHostBytesHexSome(),
                Util.marshal(SCHEMA_NAME, dfhcommarea, 72));
    }

    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformerSome() throws Exception {

        DfhcommareaJavaToHostTransformer transformer = new DfhcommareaJavaToHostTransformer();
        assertEquals(VararcomCases.getHostBytesHexSome(),
                HostData.toHexString(transformer.transform(VararcomCases.getJavaObjectSome())));
    }

    /**
     * Marshal java data object and test host data result.
     * Completely filled case.
     * @throws Exception if marshaling fails
     */
    public void testVararcomFull() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = VararcomCases.getJavaObjectFull();

        String result = Util.marshal(SCHEMA_NAME, dfhcommarea, 1752);
        assertEquals(VararcomCases.getHostBytesHexFull().substring(0, 60),
                result.substring(0, 60));
        assertEquals(VararcomCases.getHostBytesHexFull().substring(3448, 3504),
                result.substring(3448, 3504));
    }

    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformerFull() throws Exception {

        DfhcommareaJavaToHostTransformer transformer = new DfhcommareaJavaToHostTransformer();
        assertEquals(VararcomCases.getHostBytesHexFull(),
                HostData.toHexString(transformer.transform(VararcomCases.getJavaObjectFull())));
    }

}

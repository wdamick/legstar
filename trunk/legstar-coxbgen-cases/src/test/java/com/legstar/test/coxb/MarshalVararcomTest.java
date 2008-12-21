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
     * Marshal host data and test java data object result.
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
     * Marshal host data and test java data object result.
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
     * Marshal host data and test java data object result.
     * Completely filled case.
     * @throws Exception if marshaling fails
     */
    public void testVararcom() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = VararcomCases.getJavaObjectFull();

        String result = Util.marshal(SCHEMA_NAME, dfhcommarea, 1752);
        assertEquals(VararcomCases.getHostBytesHexFull().substring(0, 60),
                result.substring(0, 60));
        assertEquals(VararcomCases.getHostBytesHexFull().substring(3448, 3504),
                result.substring(3448, 3504));
    }
}

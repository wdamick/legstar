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

import java.math.BigDecimal;

import com.legstar.test.coxb.typesmix.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Marshal typesmix.
 *
 */
public class MarshalTypesmixTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "typesmix";

    /**
     * Marshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testTypesmix() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);

        dfhcommarea.setCAlphabetic("ABCDE");
        dfhcommarea.setCNational("ABCDE");
        byte[] cCDbcs = {0x0E, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x0F};
        dfhcommarea.setCDbcs(cCDbcs);
        dfhcommarea.setCAlphanumericEdited("");
        dfhcommarea.setCAlphanumeric("");
        byte[] cOctetString = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
        dfhcommarea.setCOctetString(cOctetString);
        dfhcommarea.setCPackedDecimal(new BigDecimal(0));
        dfhcommarea.setCNumericEdited1("0");
        dfhcommarea.setCNumericEdited2("0");
        dfhcommarea.setCNumericEdited3("0");
        dfhcommarea.setCNumericEdited4("0");
        dfhcommarea.setCExternalFloating("+00.00E+00");
        byte[] cIndex = {0x00, 0x00, 0x00, 0x00};
        dfhcommarea.setCIndex(cIndex);
        byte[] cPointer = {0x00, 0x00, 0x00, 0x00};
        dfhcommarea.setCPointer(cPointer);
        byte[] cProcPointer = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
        dfhcommarea.setCProcPointer(cProcPointer);
        byte[] cFuncPointer = {0x00, 0x00, 0x00, 0x00};
        dfhcommarea.setCFuncPointer(cFuncPointer);

        assertEquals("c1c2c3c4c5"
        + "004100420043004400450020002000200020"
        + "0e4040404040400f"
        + "4040404040404040404040404040"
        + "40404040404040"
        + "0000000000000000"
        + "00000000000000000000000000000000000000000c"
        + "f0f0f0f0f0f0f0f0f0f0f0f0f0c0f0"
        + "40404040404040f0404040404040404040404040404040f0"
        + "404040404040404040f040404040404040404040"
        + "00000000000000000000000000000000000000004ef0f04bf0f0c54ef0f0"
        + "00000000"
        + "0000",
                Util.marshal(SCHEMA_NAME, dfhcommarea, 176));
    }
}

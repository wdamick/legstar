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
import java.math.BigInteger;

import com.legstar.test.coxb.fixarnum.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Marshal fixarnum.
 *
 */
public class MarshalFixarnumTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "fixarnum";

    /**
     * Marshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testFixarnum() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);

        dfhcommarea.getCArrayPd().add(new BigDecimal("16534.23"));
        dfhcommarea.getCArrayPd().add(new BigDecimal("1.5"));
        dfhcommarea.getCArrayPd().add(new BigDecimal("184"));

        dfhcommarea.getCArrayZd().add(new BigDecimal("534.236"));
        dfhcommarea.getCArrayZd().add(new BigDecimal("45.007"));
        dfhcommarea.getCArrayZd().add(new BigDecimal("1.95"));

        dfhcommarea.getCArrayZi().add(new Integer("9998"));
        dfhcommarea.getCArrayZi().add(new Integer("0"));
        dfhcommarea.getCArrayZi().add(new Integer("178"));

        dfhcommarea.getCArrayBi().add(new Long("999899998"));
        dfhcommarea.getCArrayBi().add(new Long("676767"));
        dfhcommarea.getCArrayBi().add(new Long("36789013"));

        dfhcommarea.getCArrayNi().add(new BigInteger("123456789012345678"));
        dfhcommarea.getCArrayNi().add(new BigInteger("6767679998"));
        dfhcommarea.getCArrayNi().add(new BigInteger("36789184"));

        assertEquals("1653423f"
        + "0000150f"
        + "0018400f"
        + "f5f3f4f2f3f6"
        + "f0f4f5f0f0f7"
        + "f0f0f1f9f5f0"
        + "f9f9f9f8"
        + "f0f0f0f0"
        + "f0f1f7f8"
        + "3b99435e000a539f02315b1501b69b4ba630f34e00000001936299fe0000000002315bc0",
                Util.marshal(SCHEMA_NAME, dfhcommarea, 78));
    }
}

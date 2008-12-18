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

import com.legstar.test.coxb.binnatsi.LsUnsignedNative;
import com.legstar.test.coxb.binnatsi.LsDoublewords;
import com.legstar.test.coxb.binnatsi.LsFullwords;
import com.legstar.test.coxb.binnatsi.LsHalfwords;
import com.legstar.test.coxb.binnatsi.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Marshal binnatsi.
 *
 */
public class MarshalBinnatsiTest extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "binnatsi";

    /**
     * Marshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testBinnatsi() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.getJaxbObject(SCHEMA_NAME);

        LsUnsignedNative lsUnsignedNative = new LsUnsignedNative(); 

        LsHalfwords lsHalfwords = new LsHalfwords();
        lsHalfwords.setLsPs9X4High(new Short("1045"));
        lsHalfwords.setLsPs9X4Low(new Short("-128"));
        lsHalfwords.setLsPs9X4Max(new Short("32767"));
        lsHalfwords.setLsPs9X4Min(new Short("-32768"));

        LsFullwords lsFullwords = new LsFullwords();
        lsFullwords.setLsPs9X9High(123456789);
        lsFullwords.setLsPs9X9Low(-128);
        lsFullwords.setLsPs9X9Max(2147483647);
        lsFullwords.setLsPs9X9Min(-2147483648);

        LsDoublewords lsDoublewords = new LsDoublewords();
        lsDoublewords.setLsPs9X18High(17179869183L);
        lsDoublewords.setLsPs9X18Low(-4294967294L);
        lsDoublewords.setLsPs9X18Max(9223372036854775807L);
        lsDoublewords.setLsPs9X18Min(-9223372036854775808L);

        lsUnsignedNative.setLsHalfwords(lsHalfwords);
        lsUnsignedNative.setLsFullwords(lsFullwords);
        lsUnsignedNative.setLsDoublewords(lsDoublewords);

        dfhcommarea.setLsUnsignedNative(lsUnsignedNative);

        assertEquals("8000"
        + "ff80"
        + "0415"
        + "7fff"
        + "80000000"
        + "ffffff80"
        + "075bcd15"
        + "7fffffff"
        + "8000000000000000"
        + "ffffffff00000002"
        + "00000003ffffffff"
        + "7fffffffffffffff",
                Util.marshal(SCHEMA_NAME, dfhcommarea, 56));
    }
}

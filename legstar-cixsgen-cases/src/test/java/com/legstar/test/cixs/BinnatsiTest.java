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
package com.legstar.test.cixs;
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


import com.legstar.test.cixs.binnatsi.Binnatsi;
import com.legstar.test.cixs.binnatsi.BinnatsiFault;
import com.legstar.test.cixs.binnatsi.BinnatsiImpl;
import com.legstar.test.coxb.binnatsi.Dfhcommarea;
import com.legstar.test.coxb.binnatsi.LsDoublewords;
import com.legstar.test.coxb.binnatsi.LsFullwords;
import com.legstar.test.coxb.binnatsi.LsHalfwords;
import com.legstar.test.coxb.binnatsi.LsUnsignedNative;

import junit.framework.TestCase;

/**
 * Test BINNATSI adapter.
 *
 */
public class BinnatsiTest extends TestCase {

    /**
     * Try a mix of types.
     * @throws BinnatsiFault if something goes wrong.
     */
    public void testsmix() throws BinnatsiFault {
        Binnatsi port = new BinnatsiImpl(); 
        Dfhcommarea dfhcommarea = new Dfhcommarea();
        LsUnsignedNative lsUnsignedNative = new LsUnsignedNative();
        dfhcommarea.setLsUnsignedNative(lsUnsignedNative);

        LsHalfwords lsHalfworld = new LsHalfwords();
        lsUnsignedNative.setLsHalfwords(lsHalfworld);
        lsHalfworld.setLsPs9X4High(Short.parseShort("1045"));
        lsHalfworld.setLsPs9X4Low(Short.parseShort("-128"));
        lsHalfworld.setLsPs9X4Max(Short.parseShort("32767"));
        lsHalfworld.setLsPs9X4Min(Short.parseShort("-32768"));

        LsFullwords lsFullworld = new LsFullwords();
        lsUnsignedNative.setLsFullwords(lsFullworld);
        lsFullworld.setLsPs9X9High(Integer.parseInt("123456789"));
        lsFullworld.setLsPs9X9Low(Integer.parseInt("-128"));
        lsFullworld.setLsPs9X9Max(Integer.parseInt("2147483647"));
        lsFullworld.setLsPs9X9Min(Integer.parseInt("-2147483648"));

        LsDoublewords lsDoubleworld = new LsDoublewords();
        lsUnsignedNative.setLsDoublewords(lsDoubleworld);
        lsDoubleworld.setLsPs9X18High(Long.parseLong("17179869183"));
        lsDoubleworld.setLsPs9X18Low(Long.parseLong("-4294967294"));
        lsDoubleworld.setLsPs9X18Max(Long.parseLong("9223372036854775807"));
        lsDoubleworld.setLsPs9X18Min(Long.parseLong("-9223372036854775808"));

        Dfhcommarea resp = port.binnatsi(dfhcommarea, null);

        assertEquals(1045, resp.getLsUnsignedNative().getLsHalfwords().getLsPs9X4High());
        assertEquals(-128, resp.getLsUnsignedNative().getLsHalfwords().getLsPs9X4Low());
        assertEquals(32767, resp.getLsUnsignedNative().getLsHalfwords().getLsPs9X4Max());
        assertEquals(-32768, resp.getLsUnsignedNative().getLsHalfwords().getLsPs9X4Min());

        assertEquals(123456789, resp.getLsUnsignedNative().getLsFullwords().getLsPs9X9High());
        assertEquals(-128, resp.getLsUnsignedNative().getLsFullwords().getLsPs9X9Low());
        assertEquals(2147483647, resp.getLsUnsignedNative().getLsFullwords().getLsPs9X9Max());
        assertEquals(-2147483648, resp.getLsUnsignedNative().getLsFullwords().getLsPs9X9Min());

        assertEquals(17179869183L, resp.getLsUnsignedNative().getLsDoublewords().getLsPs9X18High());
        assertEquals(-4294967294L, resp.getLsUnsignedNative().getLsDoublewords().getLsPs9X18Low());
        assertEquals(9223372036854775807L, resp.getLsUnsignedNative().getLsDoublewords().getLsPs9X18Max());
        assertEquals(-9223372036854775808L, resp.getLsUnsignedNative().getLsDoublewords().getLsPs9X18Min());
    }
}

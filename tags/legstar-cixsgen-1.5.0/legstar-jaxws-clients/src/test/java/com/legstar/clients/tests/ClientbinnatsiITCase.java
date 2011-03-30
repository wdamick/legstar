/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.clients.tests;

import com.legstar.test.cixs.binnatsi.BinnatsiFault;
import com.legstar.test.cixs.binnatsi.BinnatsiPort;
import com.legstar.test.cixs.binnatsi.BinnatsiRequest;
import com.legstar.test.cixs.binnatsi.BinnatsiResponse;
import com.legstar.test.cixs.binnatsi.BinnatsiService;
import com.legstar.test.coxb.binnatsi.Dfhcommarea;
import com.legstar.test.coxb.binnatsi.LsDoublewords;
import com.legstar.test.coxb.binnatsi.LsFullwords;
import com.legstar.test.coxb.binnatsi.LsHalfwords;
import com.legstar.test.coxb.binnatsi.LsUnsignedNative;

public class ClientbinnatsiITCase extends AbstractITCase {

    public void testClient() throws BinnatsiFault {
        com.legstar.test.cixs.binnatsi.ObjectFactory wsOF = new com.legstar.test.cixs.binnatsi.ObjectFactory();
        com.legstar.test.coxb.binnatsi.ObjectFactory obOF = new com.legstar.test.coxb.binnatsi.ObjectFactory();
        BinnatsiPort port = new BinnatsiService().getBinnatsiPort();
        BinnatsiRequest req = wsOF.createBinnatsiRequest();
        Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
        req.setDfhcommarea(dfhcommarea);
        LsUnsignedNative lsUnsignedNative = obOF.createLsUnsignedNative();

        LsHalfwords lsHalfwords = obOF.createLsHalfwords();
        lsHalfwords.setLsPs9X4High(new Short("1045"));
        lsHalfwords.setLsPs9X4Low(new Short("-128"));
        lsHalfwords.setLsPs9X4Max(new Short("32767"));
        lsHalfwords.setLsPs9X4Min(new Short("-32768"));

        LsFullwords lsFullwords = obOF.createLsFullwords();
        lsFullwords.setLsPs9X9High(123456789);
        lsFullwords.setLsPs9X9Low(-128);
        lsFullwords.setLsPs9X9Max(2147483647);
        lsFullwords.setLsPs9X9Min(-2147483648);

        LsDoublewords lsDoublewords = obOF.createLsDoublewords();
        lsDoublewords.setLsPs9X18High(17179869183l);
        lsDoublewords.setLsPs9X18Low(-4294967294l);
        lsDoublewords.setLsPs9X18Max(9223372036854775807l);
        lsDoublewords.setLsPs9X18Min(-9223372036854775808l);

        lsUnsignedNative.setLsHalfwords(lsHalfwords);
        lsUnsignedNative.setLsFullwords(lsFullwords);
        lsUnsignedNative.setLsDoublewords(lsDoublewords);

        dfhcommarea.setLsUnsignedNative(lsUnsignedNative);

        BinnatsiResponse resp = port.binnatsi(req, null);
        Dfhcommarea dfhcommareaResp = resp.getDfhcommarea();

        assertEquals(1045, dfhcommareaResp.getLsUnsignedNative()
                .getLsHalfwords().getLsPs9X4High());
        assertEquals(-128, dfhcommareaResp.getLsUnsignedNative()
                .getLsHalfwords().getLsPs9X4Low());
        assertEquals(32767, dfhcommareaResp.getLsUnsignedNative()
                .getLsHalfwords().getLsPs9X4Max());
        assertEquals(-32768, dfhcommareaResp.getLsUnsignedNative()
                .getLsHalfwords().getLsPs9X4Min());

        assertEquals(123456789, dfhcommareaResp.getLsUnsignedNative()
                .getLsFullwords().getLsPs9X9High());
        assertEquals(-128, dfhcommareaResp.getLsUnsignedNative()
                .getLsFullwords().getLsPs9X9Low());
        assertEquals(2147483647, dfhcommareaResp.getLsUnsignedNative()
                .getLsFullwords().getLsPs9X9Max());
        assertEquals(-2147483648, dfhcommareaResp.getLsUnsignedNative()
                .getLsFullwords().getLsPs9X9Min());

        assertEquals(17179869183l, dfhcommareaResp.getLsUnsignedNative()
                .getLsDoublewords().getLsPs9X18High());
        assertEquals(-4294967294l, dfhcommareaResp.getLsUnsignedNative()
                .getLsDoublewords().getLsPs9X18Low());
        assertEquals(9223372036854775807l, dfhcommareaResp
                .getLsUnsignedNative().getLsDoublewords().getLsPs9X18Max());
        assertEquals(-9223372036854775808l, dfhcommareaResp
                .getLsUnsignedNative().getLsDoublewords().getLsPs9X18Min());

    }

}

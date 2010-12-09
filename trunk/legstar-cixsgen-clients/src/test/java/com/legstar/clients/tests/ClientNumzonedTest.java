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

import junit.framework.TestCase;

import com.legstar.test.cixs.numzoned.NumzonedFault;
import com.legstar.test.cixs.numzoned.NumzonedPort;
import com.legstar.test.cixs.numzoned.NumzonedRequest;
import com.legstar.test.cixs.numzoned.NumzonedResponse;
import com.legstar.test.cixs.numzoned.NumzonedService;
import com.legstar.test.coxb.numzoned.Dfhcommarea;

public class ClientNumzonedTest extends TestCase {

    public void testClient() throws NumzonedFault {
        com.legstar.test.cixs.numzoned.ObjectFactory wsOF =
                new com.legstar.test.cixs.numzoned.ObjectFactory();
        com.legstar.test.coxb.numzoned.ObjectFactory obOF =
                new com.legstar.test.coxb.numzoned.ObjectFactory();
        NumzonedPort port = new NumzonedService().getNumzonedPort();
        NumzonedRequest req = wsOF.createNumzonedRequest();
        Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
        req.setDfhcommarea(dfhcommarea);

        dfhcommarea.setLU(6);
        dfhcommarea.setLS(Short.parseShort("-5"));
        dfhcommarea.setLSSignL(Short.parseShort("-78"));
        dfhcommarea.setLSSignT(Short.parseShort("1"));
        dfhcommarea.setLSSignSL(Short.parseShort("9"));
        dfhcommarea.setLSSignST(Short.parseShort("-11"));

        NumzonedResponse resp = port.numzoned(req, null);
        Dfhcommarea dfhcommareaResp = resp.getDfhcommarea();

        assertEquals(2, dfhcommareaResp.getLU());
        assertEquals(5, dfhcommareaResp.getLS());
        assertEquals(78, dfhcommareaResp.getLSSignL());
        assertEquals(-1, dfhcommareaResp.getLSSignT());
        assertEquals(-9, dfhcommareaResp.getLSSignSL());
        assertEquals(11, dfhcommareaResp.getLSSignST());
    }

}

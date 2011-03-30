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

import com.legstar.test.cixs.fixarcom.FixarcomFault;
import com.legstar.test.cixs.fixarcom.FixarcomPort;
import com.legstar.test.cixs.fixarcom.FixarcomRequest;
import com.legstar.test.cixs.fixarcom.FixarcomResponse;
import com.legstar.test.cixs.fixarcom.FixarcomService;
import com.legstar.test.coxb.fixarcom.CArray;
import com.legstar.test.coxb.fixarcom.Dfhcommarea;

public class ClientfixarcomITCase extends AbstractITCase {

    public void testClient() throws FixarcomFault {
        com.legstar.test.cixs.fixarcom.ObjectFactory wsOF = new com.legstar.test.cixs.fixarcom.ObjectFactory();
        com.legstar.test.coxb.fixarcom.ObjectFactory obOF = new com.legstar.test.coxb.fixarcom.ObjectFactory();
        FixarcomPort port = new FixarcomService().getFixarcomPort();
        FixarcomRequest req = wsOF.createFixarcomRequest();
        Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
        req.setDfhcommarea(dfhcommarea);

        for (int i = 0; i < 7; i++) {
            CArray cArray = obOF.createCArray();
            cArray.setCItem1("ABCDE");
            cArray.setCItem2((new Integer(i)).shortValue());
            dfhcommarea.getCArray().add(cArray);
        }

        FixarcomResponse resp = port.fixarcom(req, null);
        Dfhcommarea dfhcommareaResp = resp.getDfhcommarea();

        for (int i = 0; i < 7; i++) {
            assertEquals("FGHIJ", dfhcommareaResp.getCArray().get(i)
                    .getCItem1());
            assertEquals(i + 8, dfhcommareaResp.getCArray().get(i).getCItem2());
        }
    }

}

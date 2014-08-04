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

import com.legstar.test.cixs.fixarsim.FixarsimFault;
import com.legstar.test.cixs.fixarsim.FixarsimPort;
import com.legstar.test.cixs.fixarsim.FixarsimRequest;
import com.legstar.test.cixs.fixarsim.FixarsimResponse;
import com.legstar.test.cixs.fixarsim.FixarsimService;
import com.legstar.test.coxb.fixarsim.Dfhcommarea;

public class ClientfixarsimITCase extends AbstractITCase {

    public void testClient() throws FixarsimFault {
        com.legstar.test.cixs.fixarsim.ObjectFactory wsOF = new com.legstar.test.cixs.fixarsim.ObjectFactory();
        com.legstar.test.coxb.fixarsim.ObjectFactory obOF = new com.legstar.test.coxb.fixarsim.ObjectFactory();
        FixarsimPort port = new FixarsimService().getFixarsimPort();
        FixarsimRequest req = wsOF.createFixarsimRequest();
        Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
        req.setDfhcommarea(dfhcommarea);

        for (int i = 0; i < 3; i++) {
            dfhcommarea.getCArray().add(String.format("ABCD%d", (i + 1)));
        }

        FixarsimResponse resp = port.fixarsim(req, null);
        Dfhcommarea dfhcommareaResp = resp.getDfhcommarea();

        for (int i = 0; i < 3; i++) {

            assertEquals(String.format("%dEFGH", (i + 1)), dfhcommareaResp
                    .getCArray().get(i));
        }
    }

}

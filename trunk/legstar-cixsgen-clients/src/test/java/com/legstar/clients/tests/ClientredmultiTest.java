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
import com.legstar.test.cixs.redmulti.*;
import com.legstar.test.coxb.redmulti.*;

public class ClientredmultiTest extends TestCase {

    public void testClient() throws RedmultiFault {
        com.legstar.test.cixs.redmulti.ObjectFactory wsOF =
                new com.legstar.test.cixs.redmulti.ObjectFactory();
        com.legstar.test.coxb.redmulti.ObjectFactory obOF =
                new com.legstar.test.coxb.redmulti.ObjectFactory();
        RedmultiPort port = new RedmultiService().getRedmultiPort();
        RedmultiRequest req = wsOF.createRedmultiRequest();
        Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
        req.setDfhcommarea(dfhcommarea);

        /*
         * Because input structure is identical to output structure, we
         * need to make a choice on input beween the multiple redefines.
         */
        dfhcommarea.setCData("");

        RedmultiResponse resp = port.redmulti(req, null);
        Dfhcommarea dfhcommareaResp = resp.getDfhcommarea();

        if (dfhcommareaResp.getCOutputType().compareTo("normal") == 0) {
            assertEquals("ABJADHAOUAZ", dfhcommareaResp.getFiller35()
                    .getCString());
        } else {
            assertEquals("RANDOM WAS SMALLER THAN 0.5", dfhcommareaResp
                    .getFiller38().getCErrorDescription().trim());
        }
    }

}

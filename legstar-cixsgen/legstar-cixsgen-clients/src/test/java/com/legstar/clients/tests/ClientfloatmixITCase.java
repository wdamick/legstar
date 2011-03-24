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

import com.legstar.test.cixs.floatmix.FloatmixFault;
import com.legstar.test.cixs.floatmix.FloatmixPort;
import com.legstar.test.cixs.floatmix.FloatmixRequest;
import com.legstar.test.cixs.floatmix.FloatmixResponse;
import com.legstar.test.cixs.floatmix.FloatmixService;
import com.legstar.test.coxb.floatmix.Dfhcommarea;

public class ClientfloatmixITCase extends AbstractITCase {

    public void testClient() throws FloatmixFault {
        com.legstar.test.cixs.floatmix.ObjectFactory wsOF = new com.legstar.test.cixs.floatmix.ObjectFactory();
        com.legstar.test.coxb.floatmix.ObjectFactory obOF = new com.legstar.test.coxb.floatmix.ObjectFactory();
        FloatmixPort port = new FloatmixService().getFloatmixPort();
        FloatmixRequest req = wsOF.createFloatmixRequest();
        Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
        req.setDfhcommarea(dfhcommarea);

        dfhcommarea.setCFloat0(0f);
        dfhcommarea.setCFloat1(1f);
        dfhcommarea.setCFloat1234(1234f);
        dfhcommarea.setCFloat345006P5678(345006.5678f);
        dfhcommarea.setCFloat3P40282347Ep38(3.40282347E+38f);
        dfhcommarea.setCFloat798P20067Em16(798.20067E-16f);

        FloatmixResponse resp = port.floatmix(req, null);
        Dfhcommarea dfhcommareaResp = resp.getDfhcommarea();

        assertEquals(0f, dfhcommareaResp.getCFloat0());
        assertEquals(1f, dfhcommareaResp.getCFloat1());
        assertEquals(1234f, dfhcommareaResp.getCFloat1234());
        assertEquals(345006.56779999996f,
                dfhcommareaResp.getCFloat345006P5678());
        assertEquals(3.40282347E+38f, dfhcommareaResp.getCFloat3P40282347Ep38());
        assertEquals(7.982005E-14f, dfhcommareaResp.getCFloat798P20067Em16());
    }

}

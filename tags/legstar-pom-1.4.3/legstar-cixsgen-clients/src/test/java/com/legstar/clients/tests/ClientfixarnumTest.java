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
import com.legstar.test.cixs.fixarnum.*;
import com.legstar.test.coxb.fixarnum.*;
import java.math.BigDecimal;

public class ClientfixarnumTest extends TestCase {

    public void testClient() throws FixarnumFault {
        com.legstar.test.cixs.fixarnum.ObjectFactory wsOF =
                new com.legstar.test.cixs.fixarnum.ObjectFactory();
        com.legstar.test.coxb.fixarnum.ObjectFactory obOF =
                new com.legstar.test.coxb.fixarnum.ObjectFactory();
        FixarnumPort port = new FixarnumService().getFixarnumPort();
        FixarnumRequest req = wsOF.createFixarnumRequest();
        Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
        req.setDfhcommarea(dfhcommarea);

        for (int i = 0; i < 3; i++) {
            dfhcommarea.getCArrayPd().add(
                    (new BigDecimal(i)).multiply(new BigDecimal("3.5")));
            dfhcommarea.getCArrayZd().add(
                    (new BigDecimal(i)).multiply(new BigDecimal("7.3")));
            dfhcommarea.getCArrayZi().add(i * 4);
            dfhcommarea.getCArrayBi().add(new Long(i * 457));
            dfhcommarea.getCArrayNi().add(
                    (new BigDecimal(i * 32756)).toBigInteger());
        }

        FixarnumResponse resp = port.fixarnum(req, null);
        Dfhcommarea dfhcommareaResp = resp.getDfhcommarea();

        for (int i = 0; i < 3; i++) {
            assertEquals((new BigDecimal((i + 1) * 3.5)).setScale(2),
                    dfhcommareaResp.getCArrayPd().get(i));
            BigDecimal zd = new BigDecimal("7.300");
            assertEquals((new BigDecimal((i + 1)).multiply(zd)),
                    dfhcommareaResp.getCArrayZd().get(i));
            assertEquals(new Integer(((i + 1) * 4)), dfhcommareaResp
                    .getCArrayZi().get(i));
            assertEquals(new Long((i + 1) * 457), dfhcommareaResp.getCArrayBi()
                    .get(i));
            assertEquals((new BigDecimal((i + 1) * 32756)).toBigInteger(),
                    dfhcommareaResp.getCArrayNi().get(i));
        }

    }

}

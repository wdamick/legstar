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
package com.legstar.clients.tests;

import junit.framework.TestCase;
import com.legstar.test.cixs.redopera.*;
import com.legstar.test.coxb.redopera.*;

public class ClientredoperaTest extends TestCase {
	
	public void testClientStringMethod() throws RedoperaFault{
		com.legstar.test.cixs.redopera.ObjectFactory wsOF =
		    new com.legstar.test.cixs.redopera.ObjectFactory();
		com.legstar.test.coxb.redopera.ObjectFactory obOF =
		    new com.legstar.test.coxb.redopera.ObjectFactory();
		RedoperaPort port = new RedoperaService().getRedoperaPort();
		RedoperaRequest req = wsOF.createRedoperaRequest();
		Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
		req.setDfhcommarea(dfhcommarea);
		
		dfhcommarea.setCFunction("stringMethod");
		/* Because input structure is identical to output structure, we
		 * need to make a choice on input beween the multiple redefines. */
		dfhcommarea.setCData("");
		
		RedoperaResponse resp = port.redopera(req, null);
		Dfhcommarea dfhcommareaResp = resp.getDfhcommarea();
		
		assertEquals("ABJADHAOUAZ", dfhcommareaResp.getFiller25().getCString());
	}

	public void testClientIntMethod() throws RedoperaFault{
		com.legstar.test.cixs.redopera.ObjectFactory wsOF =
		    new com.legstar.test.cixs.redopera.ObjectFactory();
		com.legstar.test.coxb.redopera.ObjectFactory obOF =
		    new com.legstar.test.coxb.redopera.ObjectFactory();
		RedoperaPort port = new RedoperaService().getRedoperaPort();
		RedoperaRequest req = wsOF.createRedoperaRequest();
		Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
		req.setDfhcommarea(dfhcommarea);
		
		dfhcommarea.setCFunction("intMethod");
		/* Because input structure is identical to output structure, we
		 * need to make a choice on input beween the multiple redefines. */
		dfhcommarea.setCData("");
		
		RedoperaResponse resp = port.redopera(req, null);
		Dfhcommarea dfhcommareaResp = resp.getDfhcommarea();
		
		assertEquals(null, dfhcommareaResp.getFiller25());
		assertEquals(345, dfhcommareaResp.getFiller28().getCInteger());
	}
}

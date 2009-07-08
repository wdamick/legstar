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
import com.legstar.test.cixs.redinout.*;
import com.legstar.test.coxb.redinout.*;

public class ClientredinoutTest extends TestCase {
	
	public void testClient() throws RedinoutFault{
		com.legstar.test.cixs.redinout.ObjectFactory wsOF =
		    new com.legstar.test.cixs.redinout.ObjectFactory();
		com.legstar.test.coxb.redinout.ObjectFactory obOF =
		    new com.legstar.test.coxb.redinout.ObjectFactory();
		RedinoutPort port = new RedinoutService().getRedinoutPort();
		RedinoutRequest req = wsOF.createRedinoutRequest();
		Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
		req.setDfhcommarea(dfhcommarea);
		
		CParain cParain = obOF.createCParain();
		cParain.setCSomeInput("FIFTEEN CHARACT");
		dfhcommarea.setCParain(cParain);
		
		RedinoutResponse resp = port.redinout(req, null);
		Dfhcommarea dfhcommareaResp = resp.getDfhcommarea();
		
		assertEquals(14082006, dfhcommareaResp.getCParaout().getCSomeOutput());
		assertEquals(null, dfhcommareaResp.getCParain());
	}

}

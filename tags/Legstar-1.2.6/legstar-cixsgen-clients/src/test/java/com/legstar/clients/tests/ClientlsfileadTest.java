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
import com.legstar.test.cixs.lsfilead.*;
import com.legstar.test.coxb.lsfilead.*;

public class ClientlsfileadTest extends TestCase {
	
	public void testClient() throws LsfileadFault{
		com.legstar.test.cixs.lsfilead.ObjectFactory wsOF =
		    new com.legstar.test.cixs.lsfilead.ObjectFactory();
		com.legstar.test.coxb.lsfilead.ObjectFactory obOF =
		    new com.legstar.test.coxb.lsfilead.ObjectFactory();
		LsfileadPort port = new LsfileadService().getLsfileadPort();
		LsfileadRequest req = wsOF.createLsfileadRequest();
		Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
		req.setDfhcommarea(dfhcommarea);
		
		dfhcommarea.setComNumber(100);
		
		LsfileadResponse resp = port.lsfilead(req, null);
		Dfhcommarea dfhcommareaResp = resp.getDfhcommarea();
		
		assertEquals("SURREY, ENGLAND",dfhcommareaResp.getComAddress());
		assertEquals("$0100.11",dfhcommareaResp.getComAmount());
		assertEquals("26 11 81",dfhcommareaResp.getComDate());
		assertEquals("S. D. BORMAN",dfhcommareaResp.getComName());
		assertEquals(100,dfhcommareaResp.getComNumber());
		assertEquals("32156778",dfhcommareaResp.getComPhone());
		assertEquals("*********",dfhcommareaResp.getComComment());
	}

}

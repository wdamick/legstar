/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/

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
		RedoperaPort port = new RedoperaService().getRedoperaImplPort();
		RedoperaRequest req = wsOF.createRedoperaRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		
		dfhcommarea.setCFunction("stringMethod");
		/* Because input structure is identical to output structure, we
		 * need to make a choice on input beween the multiple redefines. */
		dfhcommarea.setCData("");
		
		RedoperaResponse resp = port.redopera(req, null);
		DfhcommareaType dfhcommareaResp = resp.getResponse();
		
		assertEquals("ABJADHAOUAZ", dfhcommareaResp.getFiller25().getCString());
	}

	public void testClientIntMethod() throws RedoperaFault{
		com.legstar.test.cixs.redopera.ObjectFactory wsOF =
		    new com.legstar.test.cixs.redopera.ObjectFactory();
		com.legstar.test.coxb.redopera.ObjectFactory obOF =
		    new com.legstar.test.coxb.redopera.ObjectFactory();
		RedoperaPort port = new RedoperaService().getRedoperaImplPort();
		RedoperaRequest req = wsOF.createRedoperaRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		
		dfhcommarea.setCFunction("intMethod");
		/* Because input structure is identical to output structure, we
		 * need to make a choice on input beween the multiple redefines. */
		dfhcommarea.setCData("");
		
		RedoperaResponse resp = port.redopera(req, null);
		DfhcommareaType dfhcommareaResp = resp.getResponse();
		
		assertEquals(null, dfhcommareaResp.getFiller25());
		assertEquals(345, dfhcommareaResp.getFiller28().getCInteger());
	}
}

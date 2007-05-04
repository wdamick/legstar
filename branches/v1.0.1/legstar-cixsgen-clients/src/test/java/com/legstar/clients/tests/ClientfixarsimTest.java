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
import com.legstar.test.cixs.fixarsim.*;
import com.legstar.test.coxb.fixarsim.*;

public class ClientfixarsimTest extends TestCase {
	
	public void testClient() throws FixarsimFault{
		com.legstar.test.cixs.fixarsim.ObjectFactory wsOF =
		    new com.legstar.test.cixs.fixarsim.ObjectFactory();
		com.legstar.test.coxb.fixarsim.ObjectFactory obOF =
		    new com.legstar.test.coxb.fixarsim.ObjectFactory();
		FixarsimPort port = new FixarsimService().getFixarsimImplPort();
		FixarsimRequest req = wsOF.createFixarsimRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		
		for (int i = 0; i < 3; i++) {
			dfhcommarea.getCArray().add(String.format("ABCD%d", (i + 1)));
		}

		FixarsimResponse resp = port.fixarsim(req, null);
		DfhcommareaType dfhcommareaResp = resp.getResponse();

		for (int i = 0; i < 3; i++) {
			
			assertEquals(String.format("%dEFGH", (i + 1)), dfhcommareaResp.getCArray().get(i));
		}
	}

}

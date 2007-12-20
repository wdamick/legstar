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
import com.legstar.test.cixs.redinout.*;
import com.legstar.test.coxb.redinout.*;

public class ClientredinoutTest extends TestCase {
	
	public void testClient() throws RedinoutFault{
		com.legstar.test.cixs.redinout.ObjectFactory wsOF =
		    new com.legstar.test.cixs.redinout.ObjectFactory();
		com.legstar.test.coxb.redinout.ObjectFactory obOF =
		    new com.legstar.test.coxb.redinout.ObjectFactory();
		RedinoutPort port = new RedinoutService().getRedinoutImplPort();
		RedinoutRequest req = wsOF.createRedinoutRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		
		CParainType cParainType = obOF.createCParainType();
		cParainType.setCSomeInput("FIFTEEN CHARACT");
		dfhcommarea.setCParain(cParainType);
		
		RedinoutResponse resp = port.redinout(req, null);
		DfhcommareaType dfhcommareaResp = resp.getResponse();
		
		assertEquals(14082006, dfhcommareaResp.getCParaout().getCSomeOutput());
		assertEquals(null, dfhcommareaResp.getCParain());
	}

}

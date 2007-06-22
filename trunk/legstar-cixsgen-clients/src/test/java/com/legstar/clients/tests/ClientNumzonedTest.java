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

import com.legstar.test.cixs.numzoned.NumzonedFault;
import com.legstar.test.cixs.numzoned.NumzonedPort;
import com.legstar.test.cixs.numzoned.NumzonedRequest;
import com.legstar.test.cixs.numzoned.NumzonedResponse;
import com.legstar.test.cixs.numzoned.NumzonedService;
import com.legstar.test.coxb.numzoned.DfhcommareaType;

public class ClientNumzonedTest extends TestCase {
	
	public void testClient() throws NumzonedFault{
		com.legstar.test.cixs.numzoned.ObjectFactory wsOF =
		    new com.legstar.test.cixs.numzoned.ObjectFactory();
		com.legstar.test.coxb.numzoned.ObjectFactory obOF =
		    new com.legstar.test.coxb.numzoned.ObjectFactory();
		NumzonedPort port = new NumzonedService().getNumzonedImplPort();
		NumzonedRequest req = wsOF.createNumzonedRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		
		dfhcommarea.setLU(6);
		dfhcommarea.setLS(Short.parseShort("-5"));
		dfhcommarea.setLSSignL(Short.parseShort("-78"));
		dfhcommarea.setLSSignT(Short.parseShort("1"));
		dfhcommarea.setLSSignSL(Short.parseShort("9"));
		dfhcommarea.setLSSignST(Short.parseShort("-11"));

		NumzonedResponse resp = port.numzoned(req, null);
		DfhcommareaType dfhcommareaResp = resp.getResponse();

		assertEquals(2, dfhcommareaResp.getLU());
		assertEquals(5, dfhcommareaResp.getLS());
		assertEquals(78, dfhcommareaResp.getLSSignL());
		assertEquals(-1, dfhcommareaResp.getLSSignT());
		assertEquals(-9, dfhcommareaResp.getLSSignSL());
		assertEquals(11, dfhcommareaResp.getLSSignST());
	}

}

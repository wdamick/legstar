/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/

package com.legstar.clients.tests;

import junit.framework.TestCase;
import com.legstar.test.cixs.redsimpt.*;
import com.legstar.test.coxb.redsimpt.*;

public class ClientredsimptTest extends TestCase {
	
	public void testClient() throws RedsimptFault{
		com.legstar.test.cixs.redsimpt.ObjectFactory wsOF =
		    new com.legstar.test.cixs.redsimpt.ObjectFactory();
		com.legstar.test.coxb.redsimpt.ObjectFactory obOF =
		    new com.legstar.test.coxb.redsimpt.ObjectFactory();
		RedsimptPort port = new RedsimptService().getRedsimptImplPort();
		RedsimptRequest req = wsOF.createRedsimptRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		
		dfhcommarea.setCDefinition1("012345678912345678");
		
		RedsimptResponse resp = port.redsimpt(req, null);
		DfhcommareaType dfhcommareaResp = resp.getResponse();
		
		/* The choice selector effect is to produce the numeric alternative
		 * since the content is entirely digits.  */
		assertEquals(null,dfhcommareaResp.getCDefinition1());
		assertEquals(new Long(12345678912345678l), dfhcommareaResp.getCDefinition2());
	}

}

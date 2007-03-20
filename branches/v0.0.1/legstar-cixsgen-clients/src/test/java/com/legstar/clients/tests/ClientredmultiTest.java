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
import com.legstar.test.cixs.redmulti.*;
import com.legstar.test.coxb.redmulti.*;

public class ClientredmultiTest extends TestCase {
	
	public void testClient() throws RedmultiFault{
		com.legstar.test.cixs.redmulti.ObjectFactory wsOF =
		    new com.legstar.test.cixs.redmulti.ObjectFactory();
		com.legstar.test.coxb.redmulti.ObjectFactory obOF =
		    new com.legstar.test.coxb.redmulti.ObjectFactory();
		RedmultiPort port = new RedmultiService().getRedmultiImplPort();
		RedmultiRequest req = wsOF.createRedmultiRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		
		/* Because input structure is identical to output structure, we
		 * need to make a choice on input beween the multiple redefines. */
		dfhcommarea.setCData("");
		
		RedmultiResponse resp = port.redmulti(req, null);
		DfhcommareaType dfhcommareaResp = resp.getResponse();
		
		if (dfhcommareaResp.getCOutputType().compareTo("normal") == 0) {
			assertEquals("ABJADHAOUAZ                   ",dfhcommareaResp.getFiller35().getCString());
		} else {
			assertEquals("RANDOM WAS SMALLER THAN 0.5",dfhcommareaResp.getFiller38().getCErrorDescription().trim());
		}
	}

}

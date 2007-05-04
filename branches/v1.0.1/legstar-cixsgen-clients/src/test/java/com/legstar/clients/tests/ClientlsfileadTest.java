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
import com.legstar.test.cixs.lsfilead.*;
import com.legstar.test.coxb.lsfilead.*;

public class ClientlsfileadTest extends TestCase {
	
	public void testClient() throws LsfileadFault{
		com.legstar.test.cixs.lsfilead.ObjectFactory wsOF =
		    new com.legstar.test.cixs.lsfilead.ObjectFactory();
		com.legstar.test.coxb.lsfilead.ObjectFactory obOF =
		    new com.legstar.test.coxb.lsfilead.ObjectFactory();
		LsfileadPort port = new LsfileadService().getLsfileadImplPort();
		LsfileadRequest req = wsOF.createLsfileadRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		
		dfhcommarea.setComNumber(100);
		
		LsfileadResponse resp = port.lsfilead(req, null);
		DfhcommareaType dfhcommareaResp = resp.getResponse();
		
		assertEquals("SURREY, ENGLAND     ",dfhcommareaResp.getComAddress());
		assertEquals("$0100.11",dfhcommareaResp.getComAmount());
		assertEquals("26 11 81",dfhcommareaResp.getComDate());
		assertEquals("S. D. BORMAN        ",dfhcommareaResp.getComName());
		assertEquals(100,dfhcommareaResp.getComNumber());
		assertEquals("32156778",dfhcommareaResp.getComPhone());
		assertEquals("*********",dfhcommareaResp.getComComment());
	}

}

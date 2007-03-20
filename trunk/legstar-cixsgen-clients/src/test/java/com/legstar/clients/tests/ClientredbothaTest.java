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
import com.legstar.test.cixs.redbotha.*;
import com.legstar.test.coxb.redbotha.*;

public class ClientredbothaTest extends TestCase {
	
	public void testClientAlternative1() throws RedbothaFault{
		com.legstar.test.cixs.redbotha.ObjectFactory wsOF =
		    new com.legstar.test.cixs.redbotha.ObjectFactory();
		com.legstar.test.coxb.redbotha.ObjectFactory obOF =
		    new com.legstar.test.coxb.redbotha.ObjectFactory();
		RedbothaPort port = new RedbothaService().getRedbothaImplPort();
		RedbothaRequest req = wsOF.createRedbothaRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		
		Filler22Type filler22 = obOF.createFiller22Type();
		filler22.setCLeftByte("A");
		filler22.setCRightByte("B");
		dfhcommarea.setFiller22(filler22);
		
		RedbothaResponse resp = port.redbotha(req, null);
		DfhcommareaType dfhcommareaResp = resp.getResponse();
		
		/* The effect of the choice selector is to produce both alternatives */
		assertEquals(49602, dfhcommareaResp.getCNumeric().intValue());
		assertEquals("A", dfhcommareaResp.getFiller22().getCLeftByte());
		assertEquals("B", dfhcommareaResp.getFiller22().getCRightByte());
	}

	public void testClientAlternative2() throws RedbothaFault{
		com.legstar.test.cixs.redbotha.ObjectFactory wsOF =
		    new com.legstar.test.cixs.redbotha.ObjectFactory();
		com.legstar.test.coxb.redbotha.ObjectFactory obOF =
		    new com.legstar.test.coxb.redbotha.ObjectFactory();
		RedbothaPort port = new RedbothaService().getRedbothaImplPort();
		RedbothaRequest req = wsOF.createRedbothaRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		
		dfhcommarea.setCNumeric(55256);
		
		RedbothaResponse resp = port.redbotha(req, null);
		DfhcommareaType dfhcommareaResp = resp.getResponse();
		
		/* The effect of the choice selector is to produce both alternatives */
		assertEquals(55256, dfhcommareaResp.getCNumeric().intValue());
		assertEquals("P", dfhcommareaResp.getFiller22().getCLeftByte());
		assertEquals("Q", dfhcommareaResp.getFiller22().getCRightByte());
	}
}

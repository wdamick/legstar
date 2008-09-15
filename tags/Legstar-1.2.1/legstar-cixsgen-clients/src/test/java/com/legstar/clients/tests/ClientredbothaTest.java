/*******************************************************************************
 * Copyright (c) 2008 LegSem.
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

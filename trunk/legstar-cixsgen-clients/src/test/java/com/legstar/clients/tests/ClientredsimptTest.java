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
import com.legstar.test.cixs.redsimpt.*;
import com.legstar.test.coxb.redsimpt.*;

public class ClientredsimptTest extends TestCase {
	
	public void testClient() throws RedsimptFault{
		com.legstar.test.cixs.redsimpt.ObjectFactory wsOF =
		    new com.legstar.test.cixs.redsimpt.ObjectFactory();
		com.legstar.test.coxb.redsimpt.ObjectFactory obOF =
		    new com.legstar.test.coxb.redsimpt.ObjectFactory();
		RedsimptPort port = new RedsimptService().getRedsimptPort();
		RedsimptRequest req = wsOF.createRedsimptRequest();
		Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
		req.setDfhcommarea(dfhcommarea);
		
		dfhcommarea.setCDefinition1("012345678912345678");
		
		RedsimptResponse resp = port.redsimpt(req, null);
		Dfhcommarea dfhcommareaResp = resp.getDfhcommarea();
		
		/* The choice selector effect is to produce the numeric alternative
		 * since the content is entirely digits.  */
		assertEquals(null,dfhcommareaResp.getCDefinition1());
		assertEquals(new Long(12345678912345678l), dfhcommareaResp.getCDefinition2());
	}

}

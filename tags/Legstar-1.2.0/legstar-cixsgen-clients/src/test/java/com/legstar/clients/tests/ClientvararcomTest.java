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
import com.legstar.test.cixs.vararcom.*;
import com.legstar.test.coxb.vararcom.*;

public class ClientvararcomTest extends TestCase {
	
	public void testClient() throws VararcomFault{
		com.legstar.test.cixs.vararcom.ObjectFactory wsOF =
		    new com.legstar.test.cixs.vararcom.ObjectFactory();
		com.legstar.test.coxb.vararcom.ObjectFactory obOF =
		    new com.legstar.test.coxb.vararcom.ObjectFactory();
		VararcomPort port = new VararcomService().getVararcomImplPort();
		VararcomRequest req = wsOF.createVararcomRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		
		dfhcommarea.setCItemsNumber(new Short("5"));
		for (int i = 0; i < 5; i++) {
			CArrayType cArrayType = obOF.createCArrayType();
			cArrayType.setCItem1("ABCDE");
			cArrayType.setCItem2( (new Integer(i + 1)).shortValue());
			dfhcommarea.getCArray().add(cArrayType);
		}
		
		VararcomResponse resp = port.vararcom(req, null);
		DfhcommareaType dfhcommareaResp = resp.getResponse();
		
		assertEquals(36, dfhcommareaResp.getCItemsNumber());
		
		for (int i = 0; i < 36; i++) {
			assertEquals("FGHIJ", ((CArrayType) dfhcommareaResp.getCArray().get(i)).getCItem1());
			assertEquals((new Integer((i + 1) * 5)).shortValue(), ((CArrayType) dfhcommareaResp.getCArray().get(i)).getCItem2());
		}
	}

}

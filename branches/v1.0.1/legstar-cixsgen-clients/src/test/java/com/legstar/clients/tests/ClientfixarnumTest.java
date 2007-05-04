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
import com.legstar.test.cixs.fixarnum.*;
import com.legstar.test.coxb.fixarnum.*;
import java.math.BigDecimal;

public class ClientfixarnumTest extends TestCase {
	
	public void testClient() throws FixarnumFault{
		com.legstar.test.cixs.fixarnum.ObjectFactory wsOF =
		    new com.legstar.test.cixs.fixarnum.ObjectFactory();
		com.legstar.test.coxb.fixarnum.ObjectFactory obOF =
		    new com.legstar.test.coxb.fixarnum.ObjectFactory();
		FixarnumPort port = new FixarnumService().getFixarnumImplPort();
		FixarnumRequest req = wsOF.createFixarnumRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		
		for (int i = 0; i < 3; i++) {
			dfhcommarea.getCArrayPd().add((new BigDecimal(i)).multiply(new BigDecimal("3.5")));
			dfhcommarea.getCArrayZd().add((new BigDecimal(i)).multiply(new BigDecimal("7.3")));
			dfhcommarea.getCArrayZi().add(i * 4);
			dfhcommarea.getCArrayBi().add(new Long(i * 457));
			dfhcommarea.getCArrayNi().add((new BigDecimal(i * 32756)).toBigInteger());
		}

		FixarnumResponse resp = port.fixarnum(req, null);
		DfhcommareaType dfhcommareaResp = resp.getResponse();
		
		for (int i = 0; i < 3; i++) {
			assertEquals((new BigDecimal((i + 1) * 3.5)).setScale(2),dfhcommareaResp.getCArrayPd().get(i));
			BigDecimal zd = new BigDecimal("7.300");
			assertEquals((new BigDecimal((i + 1)).multiply(zd)),dfhcommareaResp.getCArrayZd().get(i));
			assertEquals(new Integer(((i + 1) * 4)),dfhcommareaResp.getCArrayZi().get(i));
			assertEquals(new Long((i + 1) * 457),dfhcommareaResp.getCArrayBi().get(i));
			assertEquals((new BigDecimal((i + 1) * 32756)).toBigInteger(),dfhcommareaResp.getCArrayNi().get(i));
		}
		
	}

}

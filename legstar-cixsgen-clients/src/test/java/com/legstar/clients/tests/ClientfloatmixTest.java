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
import com.legstar.test.cixs.floatmix.*;
import com.legstar.test.coxb.floatmix.*;

public class ClientfloatmixTest extends TestCase {
	
	public void testClient() throws FloatmixFault{
		com.legstar.test.cixs.floatmix.ObjectFactory wsOF =
		    new com.legstar.test.cixs.floatmix.ObjectFactory();
		com.legstar.test.coxb.floatmix.ObjectFactory obOF =
		    new com.legstar.test.coxb.floatmix.ObjectFactory();
		FloatmixPort port = new FloatmixService().getFloatmixImplPort();
		FloatmixRequest req = wsOF.createFloatmixRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);

		dfhcommarea.setCFloat0(0f);
		dfhcommarea.setCFloat1(1f);
		dfhcommarea.setCFloat1234(1234f);
		dfhcommarea.setCFloat345006P5678(345006.5678f);
		dfhcommarea.setCFloat3P40282347Ep38(3.40282347E+38f);
		dfhcommarea.setCFloat798P20067Em16(798.20067E-16f);

		FloatmixResponse resp = port.floatmix(req, null);
		DfhcommareaType dfhcommareaResp = resp.getResponse();

		assertEquals(0f,dfhcommareaResp.getCFloat0());
		assertEquals(1f,dfhcommareaResp.getCFloat1());
		assertEquals(1234f,dfhcommareaResp.getCFloat1234());
		assertEquals(345006.56779999996f,dfhcommareaResp.getCFloat345006P5678());
		assertEquals(3.40282347E+38f,dfhcommareaResp.getCFloat3P40282347Ep38());
		assertEquals(7.982005E-14f,dfhcommareaResp.getCFloat798P20067Em16());
	}

}

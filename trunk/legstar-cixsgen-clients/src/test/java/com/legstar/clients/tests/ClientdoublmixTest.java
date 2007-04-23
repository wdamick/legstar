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
import com.legstar.test.cixs.doublmix.*;
import com.legstar.test.coxb.doublmix.*;
public class ClientdoublmixTest extends TestCase {
	
	public void testClient() throws DoublmixFault{
		com.legstar.test.cixs.doublmix.ObjectFactory wsOF =
		    new com.legstar.test.cixs.doublmix.ObjectFactory();
		com.legstar.test.coxb.doublmix.ObjectFactory obOF =
		    new com.legstar.test.coxb.doublmix.ObjectFactory();
		DoublmixPort port = new DoublmixService().getDoublmixImplPort();
		DoublmixRequest req = wsOF.createDoublmixRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		
		dfhcommarea.setCDouble0(0d);
		dfhcommarea.setCDouble1(1d);
		dfhcommarea.setCDouble1234(1234d);
		dfhcommarea.setCDouble345006P5678(345006.5678d);
		dfhcommarea.setCDouble3P40282347Ep38(3.40282347E+38);
		dfhcommarea.setCDouble798P20067Em16(798.20067E-16);
		
		DoublmixResponse resp = port.doublmix(req, null);
		DfhcommareaType dfhcommareaResp = resp.getResponse();
		
		assertEquals(0d,dfhcommareaResp.getCDouble0());
		assertEquals(1d,dfhcommareaResp.getCDouble1());
		assertEquals(1234d,dfhcommareaResp.getCDouble1234());
		assertEquals(345006.56779999996d,dfhcommareaResp.getCDouble345006P5678());
		assertEquals(3.40282347E+38,dfhcommareaResp.getCDouble3P40282347Ep38());
		assertEquals(7.982006699999995E-14,dfhcommareaResp.getCDouble798P20067Em16());
	}

}

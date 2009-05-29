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
		Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
		req.setRequest(dfhcommarea);
		
		dfhcommarea.setCDouble0(0d);
		dfhcommarea.setCDouble1(1d);
		dfhcommarea.setCDouble1234(1234d);
		dfhcommarea.setCDouble345006P5678(345006.5678d);
		dfhcommarea.setCDouble3P40282347Ep38(3.40282347E+38);
		dfhcommarea.setCDouble798P20067Em16(798.20067E-16);
		
		DoublmixResponse resp = port.doublmix(req, null);
		Dfhcommarea dfhcommareaResp = resp.getResponse();
		
		assertEquals(0d,dfhcommareaResp.getCDouble0());
		assertEquals(1d,dfhcommareaResp.getCDouble1());
		assertEquals(1234d,dfhcommareaResp.getCDouble1234());
		assertEquals(345006.56779999996d,dfhcommareaResp.getCDouble345006P5678());
		assertEquals(3.40282347E+38,dfhcommareaResp.getCDouble3P40282347Ep38());
		assertEquals(7.982006699999995E-14,dfhcommareaResp.getCDouble798P20067Em16());
	}

}

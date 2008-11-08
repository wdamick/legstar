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
import com.legstar.test.cixs.binarcht.*;
import com.legstar.test.coxb.binarcht.*;
import java.math.BigInteger;

public class ClientbinarchtTest extends TestCase {
	
	public void testClient() throws BinarchtFault{
		com.legstar.test.cixs.binarcht.ObjectFactory wsOF =
		    new com.legstar.test.cixs.binarcht.ObjectFactory();
		com.legstar.test.coxb.binarcht.ObjectFactory obOF =
		    new com.legstar.test.coxb.binarcht.ObjectFactory();
		BinarchtPort port = new BinarchtService().getBinarchtImplPort();
		BinarchtRequest req = wsOF.createBinarchtRequest();
		Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
		req.setRequest(dfhcommarea);
		
		LsSignedNative lsSignedNative = obOF.createLsSignedNative();
		lsSignedNative.setLsPs9X18Max(12345678901234567l);
		lsSignedNative.setLsPs9X18Min(-12345678901234567l);
		lsSignedNative.setLsPs9X4Max(new Short("32767"));
		lsSignedNative.setLsPs9X4Min(new Short("-32768"));
		lsSignedNative.setLsPs9X9Max(2147483647);
		lsSignedNative.setLsPs9X9Min(-123456789);
		
		dfhcommarea.setLsSignedNative(lsSignedNative);
		
		LsUnsignedNative lsUnsignedNative = obOF.createLsUnsignedNative();
		lsUnsignedNative.setLsP9X18Max(new BigInteger("18446744073709551615"));
		lsUnsignedNative.setLsP9X18Min(new BigInteger("0"));
		lsUnsignedNative.setLsP9X4Max(65535);
		lsUnsignedNative.setLsP9X4Min(0);
		lsUnsignedNative.setLsP9X9Max(4294967295l);
		lsUnsignedNative.setLsP9X9Min(0);

		dfhcommarea.setLsUnsignedNative(lsUnsignedNative);
		
		BinarchtResponse resp = port.binarcht(req, null);
		Dfhcommarea dfhcommareaResp = resp.getResponse();
		
		assertEquals(12345678901234567l, dfhcommareaResp.getLsSignedNative().getLsPs9X18Max());
		assertEquals(-12345678901234567l, dfhcommareaResp.getLsSignedNative().getLsPs9X18Min());
		assertEquals(32767, dfhcommareaResp.getLsSignedNative().getLsPs9X4Max());
		assertEquals(-32768, dfhcommareaResp.getLsSignedNative().getLsPs9X4Min());
		assertEquals(123456789, dfhcommareaResp.getLsSignedNative().getLsPs9X9Max());
		assertEquals(-123456789, dfhcommareaResp.getLsSignedNative().getLsPs9X9Min());
		
		assertEquals(65535, dfhcommareaResp.getLsUnsignedNative().getLsP9X4Max());
		assertEquals(0, dfhcommareaResp.getLsUnsignedNative().getLsP9X4Min());
		assertEquals(1234567890, dfhcommareaResp.getLsUnsignedNative().getLsP9X9Max());
		assertEquals(0l, dfhcommareaResp.getLsUnsignedNative().getLsP9X9Min());
		assertEquals(new BigInteger("123456789012345678"), dfhcommareaResp.getLsUnsignedNative().getLsP9X18Max());
		assertEquals(new BigInteger("0"), dfhcommareaResp.getLsUnsignedNative().getLsP9X18Min());
	}

}

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
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		
		LsSignedNativeType lsSignedNativeType = obOF.createLsSignedNativeType();
		lsSignedNativeType.setLsPs9X18Max(12345678901234567l);
		lsSignedNativeType.setLsPs9X18Min(-12345678901234567l);
		lsSignedNativeType.setLsPs9X4Max(new Short("32767"));
		lsSignedNativeType.setLsPs9X4Min(new Short("-32768"));
		lsSignedNativeType.setLsPs9X9Max(2147483647);
		lsSignedNativeType.setLsPs9X9Min(-123456789);
		
		dfhcommarea.setLsSignedNative(lsSignedNativeType);
		
		LsUnsignedNativeType lsUnsignedNativeType = obOF.createLsUnsignedNativeType();
		lsUnsignedNativeType.setLsP9X18Max(new BigInteger("18446744073709551615"));
		lsUnsignedNativeType.setLsP9X18Min(new BigInteger("0"));
		lsUnsignedNativeType.setLsP9X4Max(65535);
		lsUnsignedNativeType.setLsP9X4Min(0);
		lsUnsignedNativeType.setLsP9X9Max(4294967295l);
		lsUnsignedNativeType.setLsP9X9Min(0);

		dfhcommarea.setLsUnsignedNative(lsUnsignedNativeType);
		
		BinarchtResponse resp = port.binarcht(req, null);
		DfhcommareaType dfhcommareaResp = resp.getResponse();
		
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

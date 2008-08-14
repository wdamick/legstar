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
import com.legstar.test.cixs.binpkdus.*;
import com.legstar.test.coxb.binpkdus.*;
import java.math.BigInteger;

public class ClientbinpkdusTest extends TestCase {
	
	public void testClient() throws BinpkdusFault{
		com.legstar.test.cixs.binpkdus.ObjectFactory wsOF =
		    new com.legstar.test.cixs.binpkdus.ObjectFactory();
		com.legstar.test.coxb.binpkdus.ObjectFactory obOF =
		    new com.legstar.test.coxb.binpkdus.ObjectFactory();
		BinpkdusPort port = new BinpkdusService().getBinpkdusImplPort();
		BinpkdusRequest req = wsOF.createBinpkdusRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		
		LsCompatType lsCompatType = obOF.createLsCompatType();
		lsCompatType.setLsP9X1(3);
		lsCompatType.setLsP9X18(123456789012345678l);
		lsCompatType.setLsP9X1Null(0);
		lsCompatType.setLsP9X2(12);
		lsCompatType.setLsP9X7(32769);
		
		LsExtendType lsExtendType = obOF.createLsExtendType();
		lsExtendType.setLsP9X19(new BigInteger("1234567890123456789"));
		lsExtendType.setLsP9X31(new BigInteger("1234567890123456789012345678901"));
		
		LsUnsignedPackedDecimalType lsUnsignedPackedDecimalType = obOF.createLsUnsignedPackedDecimalType();
		lsUnsignedPackedDecimalType.setLsCompat(lsCompatType);
		lsUnsignedPackedDecimalType.setLsExtend(lsExtendType);
		dfhcommarea.setLsUnsignedPackedDecimal(lsUnsignedPackedDecimalType);
		
		BinpkdusResponse resp = port.binpkdus(req, null);
		DfhcommareaType dfhcommareaResp = resp.getResponse();
		
		assertEquals(3,dfhcommareaResp.getLsUnsignedPackedDecimal().getLsCompat().getLsP9X1());
		assertEquals(123456789012345678l,dfhcommareaResp.getLsUnsignedPackedDecimal().getLsCompat().getLsP9X18());
		assertEquals(0,dfhcommareaResp.getLsUnsignedPackedDecimal().getLsCompat().getLsP9X1Null());
		assertEquals(12,dfhcommareaResp.getLsUnsignedPackedDecimal().getLsCompat().getLsP9X2());
		assertEquals(32769,dfhcommareaResp.getLsUnsignedPackedDecimal().getLsCompat().getLsP9X7());
		assertEquals(new BigInteger("1234567890123456789"),dfhcommareaResp.getLsUnsignedPackedDecimal().getLsExtend().getLsP9X19());
		assertEquals(new BigInteger("1234567890123456789012345678901"),dfhcommareaResp.getLsUnsignedPackedDecimal().getLsExtend().getLsP9X31());
	}

}

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
import com.legstar.test.cixs.binnatus.*;
import com.legstar.test.coxb.binnatus.*;
import java.math.BigInteger;

public class ClientbinnatusTest extends TestCase {
	
	public void testClient() throws BinnatusFault{
		com.legstar.test.cixs.binnatus.ObjectFactory wsOF =
		    new com.legstar.test.cixs.binnatus.ObjectFactory();
		com.legstar.test.coxb.binnatus.ObjectFactory obOF =
		    new com.legstar.test.coxb.binnatus.ObjectFactory();
		BinnatusPort port = new BinnatusService().getBinnatusImplPort();
		BinnatusRequest req = wsOF.createBinnatusRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		
		LsUnsignedNativeType lsUnsignedNativeType = obOF.createLsUnsignedNativeType(); 
		
		LsHalfwordsType lsHalfwordsType = obOF.createLsHalfwordsType();
		lsHalfwordsType.setLsP9X4High(32769);
		lsHalfwordsType.setLsP9X4Low(127);
		lsHalfwordsType.setLsP9X4Max(65535);
		lsHalfwordsType.setLsP9X4Min(0);
		
		LsFullwordsType lsFullwordsType = obOF.createLsFullwordsType();
		lsFullwordsType.setLsP9X9High(2147483649l);
		lsFullwordsType.setLsP9X9Low(65534);
		lsFullwordsType.setLsP9X9Max(4294967295l);
		lsFullwordsType.setLsP9X9Min(0);
		
		LsDoublewordsType lsDoublewordsType = obOF.createLsDoublewordsType();
		lsDoublewordsType.setLsP9X18High(new BigInteger("18446744069414584318"));
		lsDoublewordsType.setLsP9X18Low(new BigInteger("4294967294"));
		lsDoublewordsType.setLsP9X18Max(new BigInteger("18446744073709551615"));
		lsDoublewordsType.setLsP9X18Min(new BigInteger("0"));
		
		lsUnsignedNativeType.setLsHalfwords(lsHalfwordsType);
		lsUnsignedNativeType.setLsFullwords(lsFullwordsType);
		lsUnsignedNativeType.setLsDoublewords(lsDoublewordsType);
		
		dfhcommarea.setLsUnsignedNative(lsUnsignedNativeType);
		BinnatusResponse resp = port.binnatus(req, null);
		DfhcommareaType dfhcommareaResp = resp.getResponse();

		assertEquals(32769, dfhcommareaResp.getLsUnsignedNative().getLsHalfwords().getLsP9X4High());
		assertEquals(127, dfhcommareaResp.getLsUnsignedNative().getLsHalfwords().getLsP9X4Low());
		assertEquals(65535, dfhcommareaResp.getLsUnsignedNative().getLsHalfwords().getLsP9X4Max());
		assertEquals(0, dfhcommareaResp.getLsUnsignedNative().getLsHalfwords().getLsP9X4Min());
		
		assertEquals(2147483649l, dfhcommareaResp.getLsUnsignedNative().getLsFullwords().getLsP9X9High());
		assertEquals(65534, dfhcommareaResp.getLsUnsignedNative().getLsFullwords().getLsP9X9Low());
		assertEquals(4294967295l, dfhcommareaResp.getLsUnsignedNative().getLsFullwords().getLsP9X9Max());
		assertEquals(0, dfhcommareaResp.getLsUnsignedNative().getLsFullwords().getLsP9X9Min());
		
		assertEquals(new BigInteger("18446744069414584318"), dfhcommareaResp.getLsUnsignedNative().getLsDoublewords().getLsP9X18High());
		assertEquals(new BigInteger("4294967294"), dfhcommareaResp.getLsUnsignedNative().getLsDoublewords().getLsP9X18Low());
		assertEquals(new BigInteger("18446744073709551615"), dfhcommareaResp.getLsUnsignedNative().getLsDoublewords().getLsP9X18Max());
		assertEquals(new BigInteger("0"), dfhcommareaResp.getLsUnsignedNative().getLsDoublewords().getLsP9X18Min());
	}

}

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
import com.legstar.test.cixs.binnatsi.*;
import com.legstar.test.coxb.binnatsi.*;

public class ClientbinnatsiTest extends TestCase {
	
	public void testClient() throws BinnatsiFault{
		com.legstar.test.cixs.binnatsi.ObjectFactory wsOF =
		    new com.legstar.test.cixs.binnatsi.ObjectFactory();
		com.legstar.test.coxb.binnatsi.ObjectFactory obOF =
		    new com.legstar.test.coxb.binnatsi.ObjectFactory();
		BinnatsiPort port = new BinnatsiService().getBinnatsiImplPort();
		BinnatsiRequest req = wsOF.createBinnatsiRequest();
		DfhcommareaType dfhcommarea = obOF.createDfhcommareaType();
		req.setRequest(dfhcommarea);
		LsUnsignedNativeType lsUnsignedNativeType = obOF.createLsUnsignedNativeType(); 
		
		LsHalfwordsType lsHalfwordsType = obOF.createLsHalfwordsType();
		lsHalfwordsType.setLsPs9X4High(new Short("1045"));
		lsHalfwordsType.setLsPs9X4Low(new Short("-128"));
		lsHalfwordsType.setLsPs9X4Max(new Short("32767"));
		lsHalfwordsType.setLsPs9X4Min(new Short("-32768"));
		
		LsFullwordsType lsFullwordsType = obOF.createLsFullwordsType();
		lsFullwordsType.setLsPs9X9High(123456789);
		lsFullwordsType.setLsPs9X9Low(-128);
		lsFullwordsType.setLsPs9X9Max(2147483647);
		lsFullwordsType.setLsPs9X9Min(-2147483648);
		
		LsDoublewordsType lsDoublewordsType = obOF.createLsDoublewordsType();
		lsDoublewordsType.setLsPs9X18High(17179869183l);
		lsDoublewordsType.setLsPs9X18Low(-4294967294l);
		lsDoublewordsType.setLsPs9X18Max(9223372036854775807l);
		lsDoublewordsType.setLsPs9X18Min(-9223372036854775808l);
		
		lsUnsignedNativeType.setLsHalfwords(lsHalfwordsType);
		lsUnsignedNativeType.setLsFullwords(lsFullwordsType);
		lsUnsignedNativeType.setLsDoublewords(lsDoublewordsType);
		
		dfhcommarea.setLsUnsignedNative(lsUnsignedNativeType);
		
		BinnatsiResponse resp = port.binnatsi(req, null);
		DfhcommareaType dfhcommareaResp = resp.getResponse();
		
		assertEquals(1045, dfhcommareaResp.getLsUnsignedNative().getLsHalfwords().getLsPs9X4High());
		assertEquals(-128, dfhcommareaResp.getLsUnsignedNative().getLsHalfwords().getLsPs9X4Low());
		assertEquals(32767, dfhcommareaResp.getLsUnsignedNative().getLsHalfwords().getLsPs9X4Max());
		assertEquals(-32768, dfhcommareaResp.getLsUnsignedNative().getLsHalfwords().getLsPs9X4Min());
		
		assertEquals(123456789, dfhcommareaResp.getLsUnsignedNative().getLsFullwords().getLsPs9X9High());
		assertEquals(-128, dfhcommareaResp.getLsUnsignedNative().getLsFullwords().getLsPs9X9Low());
		assertEquals(2147483647, dfhcommareaResp.getLsUnsignedNative().getLsFullwords().getLsPs9X9Max());
		assertEquals(-2147483648, dfhcommareaResp.getLsUnsignedNative().getLsFullwords().getLsPs9X9Min());
		
		assertEquals(17179869183l, dfhcommareaResp.getLsUnsignedNative().getLsDoublewords().getLsPs9X18High());
		assertEquals(-4294967294l, dfhcommareaResp.getLsUnsignedNative().getLsDoublewords().getLsPs9X18Low());
		assertEquals(9223372036854775807l, dfhcommareaResp.getLsUnsignedNative().getLsDoublewords().getLsPs9X18Max());
		assertEquals(-9223372036854775808l, dfhcommareaResp.getLsUnsignedNative().getLsDoublewords().getLsPs9X18Min());
		
		
	}

}

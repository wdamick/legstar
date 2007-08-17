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
package com.legstar.test.coxb;



import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.dplarcht.DfhcommareaType;

import junit.framework.TestCase;

public class UnmarshalDplarchtTest extends TestCase {

	public void testDplarcht() throws Exception {

		String hexString   = "00015C4040404040404040404040000000000F000000000001C2C9D5C1D9C3C8E3D7D9D6C7D9C1D44040404040D5D6E3C4C5C6C9D5C5C44040000016A000000002000000000000000000000000000000000000000000000000C2C9D5D5C1E3E2";
		byte[] hostBytes = HostData.toByteArray(hexString);
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.unmarshal(hostBytes, "dplarcht");
		
		assertEquals(1,dfhcommareaType.getLsReply().getLsReplyData().getLsItemsCount());
		assertEquals("NOTDEFINED",dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramLanguage());
		assertEquals(5792,dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramLength());
		assertEquals("BINARCHT",dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramName());
		assertEquals("PROGRAM",dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramType());
		assertEquals(2,dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getLsProgramUsecount());
		assertEquals("",dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsProgramsData().getFiller113());
		assertEquals(null,dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsFilesData());
		assertEquals(null,dfhcommareaType.getLsReply().getLsReplyData().getLsItemsArray().get(0).getLsTransactionsData());
	}
}

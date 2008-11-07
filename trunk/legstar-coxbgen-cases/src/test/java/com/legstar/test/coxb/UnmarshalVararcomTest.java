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
import com.legstar.test.coxb.vararcom.Dfhcommarea;

import junit.framework.TestCase;

public class UnmarshalVararcomTest extends TestCase {

	public void testVararcom() throws Exception {

		//		              <-->
		//		              1 2   
		//		              0000             
		String hexString   = "0000";
		byte[] hostBytes = HostData.toByteArray(hexString);
		Dfhcommarea Dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "vararcom");
		
		assertEquals(0,Dfhcommarea.getCItemsNumber());
	}

	public void testVararcomSize10() throws Exception {

		//		              <--><--------><--><--------><--><--------><--><--------><--><--------><--><--------><--><--------><--><--------><--><--------><--><--------><-->
		//		              1 2 1 2 3 4 5 1 2 1 2 1 2 3 4 5 1 2 1 2 1 2 3 4 5 1 2 1 2 1 2 3 4 5 1 2 1 2 1 2 3 4 5 1 2 1 2 1 2 3 4 5 1 2 1 2 1 2 3 4 5 1 2 1 2 1 2 3 4 5 1 2 
		//		                10A B J A D    0A B J A D    7A B J A D   14A B J A D   21A B J A D   28A B J A D   35A B J A D   42A B J A D   49A B J A D   56A B J A D   63             
		String hexString   = "000ac1c2d1c1c40000c1c2d1c1c40007c1c2d1c1c4000ec1c2d1c1c40015c1c2d1c1c4001cc1c2d1c1c40023c1c2d1c1c4002ac1c2d1c1c40031c1c2d1c1c40038c1c2d1c1c4003f";
		byte[] hostBytes = HostData.toByteArray(hexString);
		Dfhcommarea Dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "vararcom");
		
		assertEquals(10,Dfhcommarea.getCItemsNumber());
		for(int i=0;i< 10; i++) {
			com.legstar.test.coxb.vararcom.CArray item = Dfhcommarea.getCArray().get(i);
			assertEquals("ABJAD",item.getCItem1());
			assertEquals(Short.parseShort(Integer.toString(7 * i)),item.getCItem2());
		}
	}
}

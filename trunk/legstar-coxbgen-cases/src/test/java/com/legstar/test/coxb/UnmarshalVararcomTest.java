package com.legstar.test.coxb;

import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.vararcom.DfhcommareaType;

import junit.framework.TestCase;

public class UnmarshalVararcomTest extends TestCase {

	public void testVararcom() throws Exception {

		//		              <-->
		//		              1 2   
		//		              0000             
		String hexString   = "0000";
		byte[] hostBytes = HostData.toByteArray(hexString);
		DfhcommareaType dfhcommarea = (DfhcommareaType) Util.unmarshal(hostBytes, "vararcom");
		
		assertEquals(0,dfhcommarea.getCItemsNumber());
	}

	public void testVararcomSize10() throws Exception {

		//		              <--><--------><--><--------><--><--------><--><--------><--><--------><--><--------><--><--------><--><--------><--><--------><--><--------><-->
		//		              1 2 1 2 3 4 5 1 2 1 2 1 2 3 4 5 1 2 1 2 1 2 3 4 5 1 2 1 2 1 2 3 4 5 1 2 1 2 1 2 3 4 5 1 2 1 2 1 2 3 4 5 1 2 1 2 1 2 3 4 5 1 2 1 2 1 2 3 4 5 1 2 
		//		                10A B J A D    0A B J A D    7A B J A D   14A B J A D   21A B J A D   28A B J A D   35A B J A D   42A B J A D   49A B J A D   56A B J A D   63             
		String hexString   = "000ac1c2d1c1c40000c1c2d1c1c40007c1c2d1c1c4000ec1c2d1c1c40015c1c2d1c1c4001cc1c2d1c1c40023c1c2d1c1c4002ac1c2d1c1c40031c1c2d1c1c40038c1c2d1c1c4003f";
		byte[] hostBytes = HostData.toByteArray(hexString);
		DfhcommareaType dfhcommarea = (DfhcommareaType) Util.unmarshal(hostBytes, "vararcom");
		
		assertEquals(10,dfhcommarea.getCItemsNumber());
		for(int i=0;i< 10; i++) {
			com.legstar.test.coxb.vararcom.CArrayType item = dfhcommarea.getCArray().get(i);
			assertEquals("ABJAD",item.getCItem1());
			assertEquals(Short.parseShort(Integer.toString(7 * i)),item.getCItem2());
		}
	}
}

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

import com.legstar.test.coxb.vararcom.DfhcommareaType;
import com.legstar.test.coxb.vararcom.CArrayType;

import junit.framework.TestCase;

public class MarshalVararcomTest extends TestCase {

	private final static String SCHEMA_NAME = "vararcom";
	
	public void testVararcomEmpty() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);
		dfhcommareaType.setCItemsNumber(Short.parseShort("0"));

		//		      <--->
		//		      1 2 
		//		      0             
		assertEquals("0000",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 2));
	}

	public void testVararcomSome() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);
		dfhcommareaType.setCItemsNumber(Short.parseShort("10"));
		for(int i=0;i < 10;i++) {
			CArrayType item = new CArrayType();
			item.setCItem1("ABJAD");
			item.setCItem2(Short.parseShort(Integer.toString(7 * i)));
			dfhcommareaType.getCArray().add(item);
		}

		//		      <--><--------><--><--------><--><--------><--><--------><--><--------><--><--------><--><--------><--><--------><--><--------><--><--------><-->
		//		      1 2 1 2 3 4 5 1 2 1 2 1 2 3 4 5 1 2 1 2 1 2 3 4 5 1 2 1 2 1 2 3 4 5 1 2 1 2 1 2 3 4 5 1 2 1 2 1 2 3 4 5 1 2 1 2 1 2 3 4 5 1 2 1 2 1 2 3 4 5 1 2 
		//		        10A B J A D    0A B J A D    7A B J A D   14A B J A D   21A B J A D   28A B J A D   35A B J A D   42A B J A D   49A B J A D   56A B J A D   63             
		assertEquals("000ac1c2d1c1c40000c1c2d1c1c40007c1c2d1c1c4000ec1c2d1c1c40015c1c2d1c1c4001cc1c2d1c1c40023c1c2d1c1c4002ac1c2d1c1c40031c1c2d1c1c40038c1c2d1c1c4003f",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 72));
	}

	public void testVararcom() throws Exception {

		// Create and populate an instance of an object (JAXB annotated)
		DfhcommareaType dfhcommareaType = (DfhcommareaType) Util.getJaxbObject(SCHEMA_NAME);
		dfhcommareaType.setCItemsNumber(Short.parseShort("250"));
		for(int i=0;i < 250;i++) {
			CArrayType item = new CArrayType();
			item.setCItem1("ABJAD");
			item.setCItem2(Short.parseShort(Integer.toString(7 * i)));
			dfhcommareaType.getCArray().add(item);
		}

		assertEquals("00fac1c2d1c1c40000c1c2d1c1c40007c1c2d1c1c4000ec1c2d1c1c40015c1c2d1c1c4001cc1c2d1c1c40023c1c2d1c1c4002ac1c2d1c1c40031c1c2d1c1c40038c1c2d1c1c4003fc1c2d1c1c40046c1c2d1c1c4004dc1c2d1c1c40054c1c2d1c1c4005bc1c2d1c1c40062c1c2d1c1c40069c1c2d1c1c40070c1c2d1c1c40077c1c2d1c1c4007ec1c2d1c1c40085c1c2d1c1c4008cc1c2d1c1c40093c1c2d1c1c4009ac1c2d1c1c400a1c1c2d1c1c400a8c1c2d1c1c400afc1c2d1c1c400b6c1c2d1c1c400bdc1c2d1c1c400c4c1c2d1c1c400cbc1c2d1c1c400d2c1c2d1c1c400d9c1c2d1c1c400e0c1c2d1c1c400e7c1c2d1c1c400eec1c2d1c1c400f5c1c2d1c1c400fcc1c2d1c1c40103c1c2d1c1c4010ac1c2d1c1c40111c1c2d1c1c40118c1c2d1c1c4011fc1c2d1c1c40126c1c2d1c1c4012dc1c2d1c1c40134c1c2d1c1c4013bc1c2d1c1c40142c1c2d1c1c40149c1c2d1c1c40150c1c2d1c1c40157c1c2d1c1c4015ec1c2d1c1c40165c1c2d1c1c4016cc1c2d1c1c40173c1c2d1c1c4017ac1c2d1c1c40181c1c2d1c1c40188c1c2d1c1c4018fc1c2d1c1c40196c1c2d1c1c4019dc1c2d1c1c401a4c1c2d1c1c401abc1c2d1c1c401b2c1c2d1c1c401b9c1c2d1c1c401c0c1c2d1c1c401c7c1c2d1c1c401cec1c2d1c1c401d5c1c2d1c1c401dcc1c2d1c1c401e3c1c2d1c1c401eac1c2d1c1c401f1c1c2d1c1c401f8c1c2d1c1c401ffc1c2d1c1c40206c1c2d1c1c4020dc1c2d1c1c40214c1c2d1c1c4021bc1c2d1c1c40222c1c2d1c1c40229c1c2d1c1c40230c1c2d1c1c40237c1c2d1c1c4023ec1c2d1c1c40245c1c2d1c1c4024cc1c2d1c1c40253c1c2d1c1c4025ac1c2d1c1c40261c1c2d1c1c40268c1c2d1c1c4026fc1c2d1c1c40276c1c2d1c1c4027dc1c2d1c1c40284c1c2d1c1c4028bc1c2d1c1c40292c1c2d1c1c40299c1c2d1c1c402a0c1c2d1c1c402a7c1c2d1c1c402aec1c2d1c1c402b5c1c2d1c1c402bcc1c2d1c1c402c3c1c2d1c1c402cac1c2d1c1c402d1c1c2d1c1c402d8c1c2d1c1c402dfc1c2d1c1c402e6c1c2d1c1c402edc1c2d1c1c402f4c1c2d1c1c402fbc1c2d1c1c40302c1c2d1c1c40309c1c2d1c1c40310c1c2d1c1c40317c1c2d1c1c4031ec1c2d1c1c40325c1c2d1c1c4032cc1c2d1c1c40333c1c2d1c1c4033ac1c2d1c1c40341c1c2d1c1c40348c1c2d1c1c4034fc1c2d1c1c40356c1c2d1c1c4035dc1c2d1c1c40364c1c2d1c1c4036bc1c2d1c1c40372c1c2d1c1c40379c1c2d1c1c40380c1c2d1c1c40387c1c2d1c1c4038ec1c2d1c1c40395c1c2d1c1c4039cc1c2d1c1c403a3c1c2d1c1c403aac1c2d1c1c403b1c1c2d1c1c403b8c1c2d1c1c403bfc1c2d1c1c403c6c1c2d1c1c403cdc1c2d1c1c403d4c1c2d1c1c403dbc1c2d1c1c403e2c1c2d1c1c403e9c1c2d1c1c403f0c1c2d1c1c403f7c1c2d1c1c403fec1c2d1c1c40405c1c2d1c1c4040cc1c2d1c1c40413c1c2d1c1c4041ac1c2d1c1c40421c1c2d1c1c40428c1c2d1c1c4042fc1c2d1c1c40436c1c2d1c1c4043dc1c2d1c1c40444c1c2d1c1c4044bc1c2d1c1c40452c1c2d1c1c40459c1c2d1c1c40460c1c2d1c1c40467c1c2d1c1c4046ec1c2d1c1c40475c1c2d1c1c4047cc1c2d1c1c40483c1c2d1c1c4048ac1c2d1c1c40491c1c2d1c1c40498c1c2d1c1c4049fc1c2d1c1c404a6c1c2d1c1c404adc1c2d1c1c404b4c1c2d1c1c404bbc1c2d1c1c404c2c1c2d1c1c404c9c1c2d1c1c404d0c1c2d1c1c404d7c1c2d1c1c404dec1c2d1c1c404e5c1c2d1c1c404ecc1c2d1c1c404f3c1c2d1c1c404fac1c2d1c1c40501c1c2d1c1c40508c1c2d1c1c4050fc1c2d1c1c40516c1c2d1c1c4051dc1c2d1c1c40524c1c2d1c1c4052bc1c2d1c1c40532c1c2d1c1c40539c1c2d1c1c40540c1c2d1c1c40547c1c2d1c1c4054ec1c2d1c1c40555c1c2d1c1c4055cc1c2d1c1c40563c1c2d1c1c4056ac1c2d1c1c40571c1c2d1c1c40578c1c2d1c1c4057fc1c2d1c1c40586c1c2d1c1c4058dc1c2d1c1c40594c1c2d1c1c4059bc1c2d1c1c405a2c1c2d1c1c405a9c1c2d1c1c405b0c1c2d1c1c405b7c1c2d1c1c405bec1c2d1c1c405c5c1c2d1c1c405ccc1c2d1c1c405d3c1c2d1c1c405dac1c2d1c1c405e1c1c2d1c1c405e8c1c2d1c1c405efc1c2d1c1c405f6c1c2d1c1c405fdc1c2d1c1c40604c1c2d1c1c4060bc1c2d1c1c40612c1c2d1c1c40619c1c2d1c1c40620c1c2d1c1c40627c1c2d1c1c4062ec1c2d1c1c40635c1c2d1c1c4063cc1c2d1c1c40643c1c2d1c1c4064ac1c2d1c1c40651c1c2d1c1c40658c1c2d1c1c4065fc1c2d1c1c40666c1c2d1c1c4066dc1c2d1c1c40674c1c2d1c1c4067bc1c2d1c1c40682c1c2d1c1c40689c1c2d1c1c40690c1c2d1c1c40697c1c2d1c1c4069ec1c2d1c1c406a5c1c2d1c1c406acc1c2d1c1c406b3c1c2d1c1c406bac1c2d1c1c406c1c1c2d1c1c406c8c1c2d1c1c406cf",
				Util.marshal(SCHEMA_NAME, dfhcommareaType, 1752));
	}
}

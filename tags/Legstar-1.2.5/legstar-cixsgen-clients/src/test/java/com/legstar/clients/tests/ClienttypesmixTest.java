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

import com.legstar.test.cixs.typesmix.TypesmixFault;
import com.legstar.test.cixs.typesmix.TypesmixPort;
import com.legstar.test.cixs.typesmix.TypesmixRequest;
import com.legstar.test.cixs.typesmix.TypesmixResponse;
import com.legstar.test.cixs.typesmix.TypesmixService;
import com.legstar.test.coxb.typesmix.Dfhcommarea;

import java.math.BigDecimal;

public class ClienttypesmixTest extends TestCase {
	
	public void testClient() throws TypesmixFault{
		com.legstar.test.cixs.typesmix.ObjectFactory wsOF = new com.legstar.test.cixs.typesmix.ObjectFactory();
		com.legstar.test.coxb.typesmix.ObjectFactory obOF = new com.legstar.test.coxb.typesmix.ObjectFactory();
		TypesmixPort port = new TypesmixService().getTypesmixImplPort();
		TypesmixRequest req = wsOF.createTypesmixRequest();
		Dfhcommarea dfhcommarea = obOF.createDfhcommarea();
		dfhcommarea.setCAlphabetic("ABCDE");
		dfhcommarea.setCNational("ABCDE");
		dfhcommarea.setCDbcs("");
		dfhcommarea.setCAlphanumericEdited("HAHAHAH");
		dfhcommarea.setCAlphanumeric("A9973");
		byte[] cOctetString = {0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};
		dfhcommarea.setCOctetString(cOctetString);
		dfhcommarea.setCSingleFloat(345.123E-09f);
		dfhcommarea.setCDoubleFloat(0.45123456789E+12d);
		dfhcommarea.setCPackedDecimal(new BigDecimal(75.45));
		dfhcommarea.setCZonedDecimal(-44535678912l);
		dfhcommarea.setCNumericEdited1("0");
		dfhcommarea.setCNumericEdited2("0");
		dfhcommarea.setCNumericEdited3("0");
		dfhcommarea.setCNumericEdited4("0");
  		byte[] cIndex = {0x00,0x00,0x00,0x00};
  		dfhcommarea.setCIndex(cIndex);
		byte[] cPointer = {0x00,0x00,0x00,0x00};
		dfhcommarea.setCPointer(cPointer);
		byte[] cProcPointer = {0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};
		dfhcommarea.setCProcPointer(cProcPointer);
		byte[] cFuncPointer = {0x00,0x00,0x00,0x00};
		dfhcommarea.setCFuncPointer(cFuncPointer);
		dfhcommarea.setCExternalFloating("+35.12E-08");
		dfhcommarea.setCBinary(-19);
		dfhcommarea.setCNativeBinary(9872);
		
		req.setRequest(dfhcommarea);
		TypesmixResponse resp = port.typesmix(req, null);
		assertEquals("FGHIJ", resp.getResponse().getCAlphabetic());
		assertEquals("", resp.getResponse().getCDbcs());
		assertEquals("AAXXX/T500   /", resp.getResponse().getCAlphanumericEdited());
		assertEquals("ALPHA12", resp.getResponse().getCAlphanumeric());
		assertEquals("0102030440404040", toHexString( resp.getResponse().getCOctetString()));
		assertEquals(65890.0f, resp.getResponse().getCSingleFloat());
		assertEquals(-5.670078E-14d, resp.getResponse().getCDoubleFloat());
		assertEquals(new BigDecimal("3456000897.56"), resp.getResponse().getCPackedDecimal());
		assertEquals(-675439650076l, resp.getResponse().getCZonedDecimal());
		assertEquals("008700CR", resp.getResponse().getCNumericEdited1());
		assertEquals("$866790/,07.678-", resp.getResponse().getCNumericEdited2());
		assertEquals("***6778.65", resp.getResponse().getCNumericEdited3());
		assertEquals("7345.505", resp.getResponse().getCNumericEdited4());
		assertEquals("00000000", toHexString( resp.getResponse().getCIndex()));
		assertEquals("00000000", toHexString( resp.getResponse().getCPointer()));
		assertEquals("0000000000000000", toHexString( resp.getResponse().getCProcPointer()));
		assertEquals("00000000", toHexString( resp.getResponse().getCFuncPointer()));
		assertEquals("+13.06E-07", resp.getResponse().getCExternalFloating());
		assertEquals(-86799, resp.getResponse().getCBinary());
		assertEquals(65001, resp.getResponse().getCNativeBinary());
	}

	/**
	 * Helper method to dump field content in hexadecimal.
	 * 
	 * @param hostBytes a byte array to get hexadecimal representation for
	 * @return a string with hexadecimal representation of the field content
	 */
	private static String toHexString(final byte[] hostBytes) {
		
		if (hostBytes == null) {
			return null;
		}
		
		StringBuffer hexString = new StringBuffer("");
		for (int i = 0; i < hostBytes.length; i++) {
			hexString.append(
					Integer.toHexString(
							hostBytes[i] & 0xFF | 0x100).substring(1, 3));
		}
		
		return hexString.toString();
	}

}

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
package com.legstar.xsdc.test.cases.cultureinfo;

import java.math.BigDecimal;
import java.util.Locale;

import junit.framework.TestCase;

public class CultureInfoTest extends TestCase {
	
	public void setUp() {
		Locale.setDefault(new Locale("fr", "FR"));
	}
	
	public void testEmptyRequest() {
		CultureInfoImpl si = new CultureInfoImpl();
		CultureInfoRequest request = new CultureInfoRequest();
		try {
			si.getInfo(request);
			fail();
		} catch (CultureInfoException e) {
			assertEquals("You must provide a CultureInfo", e.getMessage());
		}
	}

	public void testWrongCultureInfo() {
		CultureInfoImpl si = new CultureInfoImpl();
		CultureInfoRequest request = new CultureInfoRequest();
		request.setCultureCode("marzipan");
		try {
			si.getInfo(request);
			fail();
		} catch (CultureInfoException e) {
			assertEquals("CultureInfo marzipan does not conform to xx-yy format", e.getMessage());
		}
	}

	public void testFrench() {
		CultureInfoImpl si = new CultureInfoImpl();
		CultureInfoRequest request = new CultureInfoRequest();
		request.setCultureCode("fr-FR");
		request.setDecimalNumber(new BigDecimal("13125.56"));
		try {
			CultureInfoReply resp = si.getInfo(request);
			assertEquals("France", resp.getDisplayCountry());
			assertEquals("français", resp.getDisplayLanguage());
			System.out.println(resp.getFormattedDate());
			assertEquals("€", resp.getCurrencySymbol());
			assertEquals("13 125,56", resp.getFormattedDecimalNumber());
			assertEquals("fr-FR", resp.getServerCultureInfo().getCultureCode());
			assertEquals("France", resp.getServerCultureInfo().getDisplayCountry());
			assertEquals("français", resp.getServerCultureInfo().getDisplayLanguage());
		} catch (CultureInfoException e) {
			fail(e.getMessage());
		}
	}

	public void testUS() {
		CultureInfoImpl si = new CultureInfoImpl();
		CultureInfoRequest request = new CultureInfoRequest();
		request.setCultureCode("en-US");
		request.setDecimalNumber(new BigDecimal("13125.56"));
		try {
			CultureInfoReply resp = si.getInfo(request);
			assertEquals("Etats-Unis", resp.getDisplayCountry());
			assertEquals("anglais", resp.getDisplayLanguage());
			System.out.println(resp.getFormattedDate());
			assertEquals("USD", resp.getCurrencySymbol());
			assertEquals("13,125.56", resp.getFormattedDecimalNumber());
			assertEquals("fr-FR", resp.getServerCultureInfo().getCultureCode());
			assertEquals("France", resp.getServerCultureInfo().getDisplayCountry());
			assertEquals("français", resp.getServerCultureInfo().getDisplayLanguage());
		} catch (CultureInfoException e) {
			fail(e.getMessage());
		}
	}
}

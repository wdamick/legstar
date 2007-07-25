package com.legstar.c2ws.sample.test;

import java.math.BigDecimal;

import com.legstar.c2ws.sample.CultureInfoImpl;
import com.legstar.c2ws.sample.CultureInfoException;
import com.legstar.c2ws.sample.CultureInfoReply;
import com.legstar.c2ws.sample.CultureInfoRequest;

import junit.framework.TestCase;

public class ServerInfoTest extends TestCase {
	
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

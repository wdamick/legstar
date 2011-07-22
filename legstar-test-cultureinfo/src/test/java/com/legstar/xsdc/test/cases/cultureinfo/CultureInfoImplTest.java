package com.legstar.xsdc.test.cases.cultureinfo;

import java.math.BigDecimal;
import java.util.Locale;

import junit.framework.TestCase;

public class CultureInfoImplTest extends TestCase {
	
	public void test1() throws Exception {
		Locale.setDefault(Locale.US);
		
		CultureInfoImpl impl = new CultureInfoImpl();
		CultureInfoRequest request = new CultureInfoRequest();
		request.setCultureCode("eu-ES");
		request.setDecimalNumber(BigDecimal.valueOf(2568.45d));
		CultureInfoReply reply = impl.getInfo(request);
		assertEquals("EUR", reply.getCurrencySymbol());
		assertEquals("Spain", reply.getDisplayCountry());
		assertEquals("Basque", reply.getDisplayLanguage());
		assertEquals("Tuesday, October 28, 2008 1:45:00 PM CET", reply.getFormattedDate());
		assertEquals("2,568.45", reply.getFormattedDecimalNumber());
		assertEquals("fr-FR", reply.getServerCultureInfo().getCultureCode());
		assertEquals("France", reply.getServerCultureInfo().getDisplayCountry());
		assertEquals("French", reply.getServerCultureInfo().getDisplayLanguage());
	}

}

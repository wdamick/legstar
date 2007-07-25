package com.legstar.c2ws;

import java.math.BigDecimal;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.visitor.CobolMarshalVisitor;
import com.legstar.coxb.visitor.CobolUnmarshalVisitor;
import com.legstar.test.coxb.cultureinfo.CultureInfoReplyType;
import com.legstar.test.coxb.cultureinfo.CultureInfoRequestType;
import com.legstar.test.coxb.cultureinfo.ServerCultureInfoType;

public final class CultureInfoCases {

	public static byte[] getRequestHostData() throws Exception {
		com.legstar.test.coxb.cultureinfo.ObjectFactory of =
			new com.legstar.test.coxb.cultureinfo.ObjectFactory();
		
		CultureInfoRequestType o = of.createCultureInfoRequestType();
		o.setCultureCode("fr-FR");
		o.setDecimalNumber(new BigDecimal("125645.62"));

		CComplexReflectBinding ccem = new CComplexReflectBinding(of, o);

		byte[] hostBytes = new byte[ccem.calcByteLength()];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0,
				new CobolSimpleConverters(new CobolContext()));
		ccem.accept(mv);
		return mv.getHostBytes();
	}
	
	public static CultureInfoReplyType getResponseJaxbObject() throws Exception {
		CultureInfoReplyType response = new CultureInfoReplyType();
		response.setCurrencySymbol("€");
		response.setDisplayCountry("France");
		response.setDisplayLanguage("French");
		response.setFormattedDate("18 avril 1992 18:38");
		response.setFormattedDecimalNumber("125.645,62");
		ServerCultureInfoType sci = new ServerCultureInfoType();
		sci.setCultureCode("en-US");
		sci.setDisplayCountry("United States");
		sci.setDisplayLanguage("English");
		response.setServerCultureInfo(sci);
		return response;
	}

	public static CultureInfoReplyType getResponseFromHostBytes(byte[] hostBytes) throws Exception {
		com.legstar.test.coxb.cultureinfo.ObjectFactory of =
			new com.legstar.test.coxb.cultureinfo.ObjectFactory();
		CultureInfoReplyType response = new CultureInfoReplyType();
		
		CComplexReflectBinding ccem = new CComplexReflectBinding(of, response);

		CobolUnmarshalVisitor mv = new CobolUnmarshalVisitor(hostBytes, 0,
				new CobolSimpleConverters(new CobolContext()));
		ccem.accept(mv);
		return response;
	}
	

}

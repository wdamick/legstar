/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.c2ws;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.util.JAXBAnnotationException;
import com.legstar.util.JAXBElementDescriptor;
import com.legstar.coxb.visitor.CobolMarshalVisitor;
import com.legstar.coxb.visitor.CobolUnmarshalVisitor;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.LegStarHeaderPart;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarMessagePart;
import com.legstar.test.coxb.cultureinfo.CultureInfoReplyType;
import com.legstar.test.coxb.cultureinfo.CultureInfoParametersType;
import com.legstar.test.coxb.cultureinfo.ServerCultureInfoType;

public final class CultureInfoCases {

	public static byte[] getRequestHostData() throws Exception {
		com.legstar.test.coxb.cultureinfo.ObjectFactory of =
			new com.legstar.test.coxb.cultureinfo.ObjectFactory();
		
		CultureInfoParametersType o = of.createCultureInfoParametersType();
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
	
	public static C2wsWSDescriptor getWSDescriptor() throws JAXBAnnotationException {
		C2wsWSDescriptor wsd = new C2wsWSDescriptor();
		wsd.setWsdlUrl("http://localhost:8080/jaxws-cultureinfo/getinfo?wsdl");
		wsd.setWsdlTargetNamespace("http://cultureinfo.cases.test.xsdc.legstar.com/");
		wsd.setWsdlPort("CultureInfoImplPort");
		wsd.setWsdlName("CultureInfoImplService");
		JAXBElementDescriptor request = new JAXBElementDescriptor(
				"com.legstar.test.coxb.cultureinfo", "GetInfoType");
		wsd.setRequestElementDescriptor(request);
		JAXBElementDescriptor response = new JAXBElementDescriptor(
				"com.legstar.test.coxb.cultureinfo", "GetInfoResponseType");
		wsd.setResponseElementDescriptor(response);
		return wsd;
	}
	
	public static LegStarMessage getCultureInfoRequestMessage() throws Exception {
		List <LegStarMessagePart> dataParts = new ArrayList <LegStarMessagePart>();
		LegStarMessagePart inCommarea = new CommareaPart(getRequestHostData());
		dataParts.add(inCommarea);
		LegStarHeaderPart headerPart = new LegStarHeaderPart();
		headerPart.setJsonString("{\"ServiceName\":\"CultureInfo\"}");
		return new LegStarMessage(headerPart, dataParts);
	}

}

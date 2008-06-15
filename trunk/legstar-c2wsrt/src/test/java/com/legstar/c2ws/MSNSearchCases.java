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
package com.legstar.c2ws;

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
import com.legstar.test.coxb.MSNSearch.ArrayOfResultResultsType;
import com.legstar.test.coxb.MSNSearch.ArrayOfSourceRequestRequestsType;
import com.legstar.test.coxb.MSNSearch.ArrayOfSourceResponseResponsesType;
import com.legstar.test.coxb.MSNSearch.ObjectFactory;
import com.legstar.test.coxb.MSNSearch.ResultType;
import com.legstar.test.coxb.MSNSearch.SafeSearchOptionsType;
import com.legstar.test.coxb.MSNSearch.Search;
import com.legstar.test.coxb.MSNSearch.SearchRequestType;
import com.legstar.test.coxb.MSNSearch.SearchResponse;
import com.legstar.test.coxb.MSNSearch.SearchResponseType;
import com.legstar.test.coxb.MSNSearch.SourceRequestType;
import com.legstar.test.coxb.MSNSearch.SourceResponseType;
import com.legstar.test.coxb.MSNSearch.SourceTypeType;

public final class MSNSearchCases {

	private static final String LIVE_ID = "5588C3ACE949315B3ECAADDA908611BDF5D8D5AA";
	
	public static byte[] getRequestHostData() throws Exception {
		ObjectFactory of =	new ObjectFactory();
		Search search = getRequestJaxbObject();
		CComplexReflectBinding ccem = new CComplexReflectBinding(of, search);

		byte[] hostBytes = new byte[ccem.calcByteLength()];
		CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0,
				new CobolSimpleConverters(new CobolContext()));
		ccem.accept(mv);
		
		/* Trim unused trailing bytes */
		if (mv.getOffset() < hostBytes.length) {
			byte[] result = new byte[mv.getOffset()];
			System.arraycopy( hostBytes, 0, result, 0, mv.getOffset());
			return result;
		}
		return mv.getHostBytes();
	}
	
	public static SearchResponse getResponseFromHostBytes(byte[] hostBytes) throws Exception {
		ObjectFactory of =	new ObjectFactory();
		SearchResponse response = new SearchResponse();
		CComplexReflectBinding ccem = new CComplexReflectBinding(of, response);

		CobolUnmarshalVisitor mv = new CobolUnmarshalVisitor(hostBytes, 0,
				new CobolSimpleConverters(new CobolContext()));
		ccem.accept(mv);
		return response;
	}
	
	public static Search getRequestJaxbObject() throws Exception {
		ObjectFactory of =	new ObjectFactory();
		
		Search request = of.createSearch();
		SearchRequestType searchRequestType = of.createSearchRequestType();
		searchRequestType.setAppID(LIVE_ID);
		searchRequestType.setCultureInfo("en-US");
		searchRequestType.setSafeSearch(SafeSearchOptionsType.MODERATE);
		SourceRequestType srt = of.createSourceRequestType();
        srt.setSource(SourceTypeType.WEB);
        srt.setCount(1);
		ArrayOfSourceRequestRequestsType sr = of.createArrayOfSourceRequestRequestsType();
        sr.getSourceRequest().add(srt);
        searchRequestType.setRequests(sr);
        searchRequestType.setQuery("live search");
 		request.setRequest(searchRequestType);
 		return request;
	}
	
	public static SearchResponse getResponseJaxbObject() throws Exception {
		ObjectFactory of =	new ObjectFactory();
		SearchResponse response = of.createSearchResponse();
		SearchResponseType searchResponses = of.createSearchResponseType();
		
		ArrayOfSourceResponseResponsesType sourceResponses = of.createArrayOfSourceResponseResponsesType();
		
		SourceResponseType sourceResponse = of.createSourceResponseType();
		sourceResponse.setOffset(0);
		sourceResponse.setSource(SourceTypeType.WEB);
		sourceResponse.setTotal(30);

		ArrayOfResultResultsType results = of.createArrayOfResultResultsType();
		ResultType result = of.createResultType();
		result.setDescription("Study sets and science gifts from Jensan Scientifics. Your source for fantastic items of Space ... 23 Legged Starfish measures 5\" across. Great for stirring the imagination about what else can be ... ");
		result.setTitle("23 Legged Starfish");
		result.setUrl("http://www.sciencemall-usa.com/23legstar.html");
		results.getResult().add(result);

		sourceResponse.setResults(results);
		
		sourceResponses.getSourceResponse().add(sourceResponse);
		
		searchResponses.setResponses(sourceResponses);
		
		response.setResponse(searchResponses);
		
		return response;
	}

	public static C2wsWSDescriptor getWSDescriptor() throws JAXBAnnotationException {
		C2wsWSDescriptor wsd = new C2wsWSDescriptor();
		wsd.setWsdlUrl("http://soap.search.msn.com/webservices.asmx?wsdl");
		wsd.setWsdlTargetNamespace("http://schemas.microsoft.com/MSNSearch/2005/09/fex");
		wsd.setWsdlPort("MSNSearchPort");
		wsd.setWsdlName("MSNSearchService");
		JAXBElementDescriptor request = new JAXBElementDescriptor(
				"com.legstar.test.coxb.MSNSearch", "Search");
		wsd.setRequestElementDescriptor(request);
		JAXBElementDescriptor response = new JAXBElementDescriptor(
				"com.legstar.test.coxb.MSNSearch", "SearchResponse");
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

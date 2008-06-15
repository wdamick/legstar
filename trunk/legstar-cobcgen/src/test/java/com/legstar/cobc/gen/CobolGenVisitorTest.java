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
package com.legstar.cobc.gen;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringWriter;

import com.legstar.cobc.gen.CobolGenVisitor;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.host.HostException;
import com.legstar.test.coxb.MSNSearch.SourceTypeType;

import junit.framework.TestCase;

public class CobolGenVisitorTest extends TestCase {
	
	private static final boolean DEBUG_MODE = true;
	
	/** Pattern for temporary files. */
	private static final String TEMP_PATTERN = "legstar";
	
	/** Suffix for temporary files. */
	private static final String TEMP_SUFFIX = ".temp";

	private static final String LIVE_ID = "5588C3ACE949315B3ECAADDA908611BDF5D8D5AA";
	
	public void testLsfileae() throws Exception {

		File outFile = File.createTempFile(TEMP_PATTERN, TEMP_SUFFIX);
		processLsfileae(outFile);
		String source = CobcUtil.getSource(outFile, DEBUG_MODE);
		assertTrue(source.contains("01 COM-LSFILEAE."));
		assertTrue(source.contains("02 COM-NUMBER PIC 9(6)."));
		assertTrue(source.contains("02 COM-PERSONAL."));
		assertTrue(source.contains("03 COM-NAME PIC X(20)."));
	}

	public void testDplarcht() throws Exception {

		File outFile = File.createTempFile(TEMP_PATTERN, TEMP_SUFFIX);
		processDplarcht(outFile);
		String source = CobcUtil.getSource(outFile, DEBUG_MODE);
		assertTrue(source.contains("01 COM-DPLARCHT."));
		assertTrue(source.contains("03 LS-MAX-ITEMS PIC 9(4) REDEFINES LS-ALL-ITEMS."));
		assertTrue(source.contains("04 LS-ITEMS-ARRAY"));
		assertTrue(source.contains("04 LS-ITEMS-ARRAY OCCURS 1"));
		assertTrue(source.contains("TO 500 DEPENDING ON LS-ITEMS-COUNT."));
		assertTrue(source.contains("05 LS-PROGRAMS-DATA REDEFINES LS-FILES-DATA."));
	}
	
	public void testMSNSearch() throws Exception {

		File outFile = File.createTempFile(TEMP_PATTERN, TEMP_SUFFIX);
		processMSNSearch(outFile);
		String source = CobcUtil.getSource(outFile, DEBUG_MODE);
		assertTrue(source.contains("02 Flags--C PIC 9(9) BINARY."));
		assertTrue(source.contains("02 SourceRequest--C PIC 9(9) BINARY."));
		assertTrue(source.contains("03 Flags PIC X(32) OCCURS 1 TO 10 DEPENDING ON Flags--C."));
		assertTrue(source.contains("04 Longitude COMP-2."));
		assertTrue(source.contains("05 SortBy PIC X(32) OCCURS 1"));
		assertTrue(source.contains("TO 10 DEPENDING ON SortBy--C."));
	}
	
	public void testMSNSearchResponse() throws Exception {

		String source = processMSNSearchResponse();
		assertTrue(source.contains("05 COM-MSNSEARCH-RESPONSE."));
		assertTrue(source.contains("25 Results."));
		assertTrue(source.contains("30 Result OCCURS 0"));
		assertTrue(source.contains("TO 10 DEPENDING ON Result--C."));
		assertTrue(source.contains("35 SearchTags PIC X(32)."));
		assertTrue(source.contains("40 Minute PIC 9(9) COMP-5."));
		assertTrue(source.contains("40 Latitude COMP-2."));
		assertTrue(source.contains("40 ImageHeight PIC 9(9) COMP-5."));
	}

	private void processLsfileae(File outFile) throws HostException, IOException {
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.lsfileae.ObjectFactory objectFactory = new com.legstar.test.coxb.lsfileae.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.lsfileae.DfhcommareaType dfhcommarea = objectFactory.createDfhcommareaType();
		dfhcommarea.setComNumber(100);
		dfhcommarea.setComDate("100458");
		dfhcommarea.setComAmount("00100.35");
		dfhcommarea.setComComment("A VOIR");
		com.legstar.test.coxb.lsfileae.ComPersonalType personal = objectFactory.createComPersonalType();
		personal.setComName("TOTO");
		personal.setComAddress("LABAS STREET");
		personal.setComPhone("88993314");
		dfhcommarea.setComPersonal(personal);

		CComplexReflectBinding ccem = new CComplexReflectBinding(
				objectFactory, dfhcommarea);
		ccem.setCobolName("COM-LSFILEAE");
		BufferedWriter writer = new BufferedWriter(new FileWriter(outFile));
		CobolGenVisitor cev = new CobolGenVisitor(writer);
		ccem.accept(cev);
		writer.close();
	}

	private void processDplarcht(File outFile) throws HostException, IOException {
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.dplarcht.ObjectFactory objectFactory = new com.legstar.test.coxb.dplarcht.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.dplarcht.DfhcommareaType dfhcommarea = objectFactory.createDfhcommareaType();
		com.legstar.test.coxb.dplarcht.LsRequestType lsRequestType = objectFactory.createLsRequestType();
		lsRequestType.setLsAllItems("*");
		lsRequestType.setLsRequestType(0);

		CComplexReflectBinding ccem = new CComplexReflectBinding(
				objectFactory, dfhcommarea);
		ccem.setCobolName("COM-DPLARCHT");
		BufferedWriter writer = new BufferedWriter(new FileWriter(outFile));
		CobolGenVisitor cev = new CobolGenVisitor(writer);
		ccem.accept(cev);
		writer.close();
	}

	private void processMSNSearch(File outFile) throws HostException, IOException {
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.MSNSearch.ObjectFactory objectFactory = new com.legstar.test.coxb.MSNSearch.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.MSNSearch.Search search = objectFactory.createSearch();
		com.legstar.test.coxb.MSNSearch.SearchRequestType searchRequest = objectFactory.createSearchRequestType();
		searchRequest.setAppID(LIVE_ID);
		searchRequest.setCultureInfo("en-US");
		com.legstar.test.coxb.MSNSearch.ArrayOfSourceRequestRequestsType sr = objectFactory.createArrayOfSourceRequestRequestsType();
		com.legstar.test.coxb.MSNSearch.SourceRequestType srt = objectFactory.createSourceRequestType();
        srt.setSource(com.legstar.test.coxb.MSNSearch.SourceTypeType.WEB);
        srt.setCount(1);
        sr.getSourceRequest().add(srt);
        searchRequest.setRequests(sr);
        searchRequest.setQuery("live search");
 		search.setRequest(searchRequest);

		CComplexReflectBinding ccem = new CComplexReflectBinding(
				objectFactory, search);
		ccem.setCobolName("COM-MSNSEARCH");
		BufferedWriter writer = new BufferedWriter(new FileWriter(outFile));
		CobolGenVisitor cev = new CobolGenVisitor(writer);
		ccem.accept(cev);
		writer.close();
	}

	private String processMSNSearchResponse() throws HostException, IOException {
		// Create an instance of the JAXB object factory
		com.legstar.test.coxb.MSNSearch.ObjectFactory objectFactory = new com.legstar.test.coxb.MSNSearch.ObjectFactory();
		// Create and populate an instance of an object (JAXB annotated)
		com.legstar.test.coxb.MSNSearch.SearchResponse searchResponse = objectFactory.createSearchResponse();
		com.legstar.test.coxb.MSNSearch.SearchResponseType searchResponses = objectFactory.createSearchResponseType();
		com.legstar.test.coxb.MSNSearch.ArrayOfSourceResponseResponsesType sourceResponses = objectFactory.createArrayOfSourceResponseResponsesType();
		com.legstar.test.coxb.MSNSearch.SourceResponseType sourceResponse = objectFactory.createSourceResponseType();
		sourceResponse.setOffset(0);
		sourceResponse.setSource(SourceTypeType.WEB);
		sourceResponse.setTotal(30);
		com.legstar.test.coxb.MSNSearch.ArrayOfResultResultsType results = objectFactory.createArrayOfResultResultsType();
		com.legstar.test.coxb.MSNSearch.ResultType result = objectFactory.createResultType();
		result.setDescription("Study sets and science gifts from Jensan Scientifics. Your source for fantastic items of Space ... 23 Legged Starfish measures 5\" across. Great for stirring the imagination about what else can be ... ");
		result.setTitle("23 Legged Starfish");
		result.setUrl("http://www.sciencemall-usa.com/23legstar.html");
		
		results.getResult().add(result);
		sourceResponse.setResults(results);
		
		sourceResponses.getSourceResponse().add(sourceResponse);
		searchResponses.setResponses(sourceResponses);
		searchResponse.setResponse(searchResponses);
		
		CComplexReflectBinding ccem = new CComplexReflectBinding(
				objectFactory, searchResponse);
		ccem.setCobolName("COM-MSNSEARCH-RESPONSE");
		StringWriter writer = new StringWriter();
		BufferedWriter bufWriter = new BufferedWriter(writer);
		CobolGenVisitor cev = new CobolGenVisitor(3, 5, bufWriter);
		ccem.accept(cev);
		bufWriter.flush();
		System.out.print(writer.toString());

		return writer.toString();
	}

}

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
package com.legstar.test.coxb;


import java.util.List;

import junit.framework.TestCase;

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

/**
 * Provides data samples for testing throughout LegStar.
 */
public final class MSNSearchCases extends TestCase {

    /** A valid LIVE ID.*/
    private static final String LIVE_ID = "5588C3ACE949315B3ECAADDA908611BDF5D8D5AA";

    /**
     * Utility class.
     */
    private MSNSearchCases() {
    }

    /**
     * @return an instance of a valued java object.
     */
    public static Search getJaxbObjectRequest() {
        ObjectFactory of =  new ObjectFactory();

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
        searchRequestType.setQuery("LegStar");
        request.setRequest(searchRequestType);
        return request;
    }

    /**
     * @return an instance of a valued java object.
     */
    public static SearchResponse getJaxbObjectResponse() {
        ObjectFactory of =  new ObjectFactory();
        SearchResponse response = of.createSearchResponse();
        SearchResponseType searchResponses = of.createSearchResponseType();

        ArrayOfSourceResponseResponsesType sourceResponses =
            of.createArrayOfSourceResponseResponsesType();

        SourceResponseType sourceResponse = of.createSourceResponseType();
        sourceResponse.setOffset(0);
        sourceResponse.setSource(SourceTypeType.WEB);
        sourceResponse.setTotal(185);

        ArrayOfResultResultsType results = of.createArrayOfResultResultsType();
        ResultType result = of.createResultType();
        result.setDescription("Overview. LegStar is the first open-source initiative"
                + " in the domain of legacy application integration. This page will"
                + " give you a general idea of LegStar, if you need further ...");
        result.setTitle("LegStar - Overview");
        result.setUrl("http://www.legsem.com/legstar/");
        results.getResult().add(result);

        sourceResponse.setResults(results);

        sourceResponses.getSourceResponse().add(sourceResponse);

        searchResponses.setResponses(sourceResponses);

        response.setResponse(searchResponses);

        return response;
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexRequest() {
        return
        "00000000"
        + "00000000"
        + "00000000"
        + "00000000"
        + "00000001"
        + "f5f5f8f8c3f3c1c3c5f9f4f9f3f1f5c2f3c5c3c1c1c4c4c1f9f0f8f6f1f1c2c4c6f5c4f8c4f5c1c1"
        + "d38587e2a38199404040404040404040404040404040404040404040404040404040404040404040"
        + "40404040404040404040404040404040404040404040404040404040404040404040404040404040"
        + "40404040404040404040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040"
        + "859560e4e2404040404040404040404040404040404040404040404040404040"
        + "d49684859981a385404040404040404040404040404040404040404040404040"
        + "000000000000000000000000000000000000000000000000"
        + "e685824040404040404040404040404040404040404040404040404040404040"
        + "00000000"
        + "00000001"
        + "4040404040404040404040404040404040404040404040404040404040404040";
    }
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexResponse() {
        return
        "00000000"
        + "00000001"
        + "00000001"
        + "e685824040404040404040404040404040404040404040404040404040404040"
        + "00000000"
        + "000000b9"
        /*  1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0*/
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "d38587e2a38199406040d6a58599a58985a64040404040404040404040404040"
        + "d6a58599a58985a64b40d38587e2a381994089a240a3888540868999a2a3409697859560a296a499"
        + "838540899589a38981a389a58540899540a3888540849694818995409686409385878183a8408197"
        + "9793898381a3899695408995a385879981a38996954b40e38889a2409781878540a6899393408789"
        + "a58540a896a440814087859585998193408984858140968640d38587e2a381996b40898640a896a4"
        + "40958585844086a499a3888599404b4b4b4040404040404040404040404040404040404040404040"
        + "40404040404040404040404040404040404040404040404040404040404040404040404040404040"
        + "40404040404040404040404040404040"
        + "88a3a3977a6161a6a6a64b938587a285944b83969461938587a2a38199614040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "00000000"
        + "00000000"
        + "00000000"
        + "00000000"
        + "00000000"
        + "00000000"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        /*  1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0*/
        + "000000000000000000000000000000000000000000000000"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "00000000"
        + "00000000"
        + "00000000"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "00000000"
        + "00000000"
        + "00000000"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "00000000"
        + "00000000"
        + "00000000"
        + "00000000"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "00000000"
        + "00000000"
        + "00000000"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "00000000"
        + "00000000"
        + "00000000"
        + "00000000";
    }

    /**
     * Check that data object contains the expected values.
     * @param searchResponse the java object to check
     */
    public static void checkJavaObjectResponse(final SearchResponse searchResponse) {
        List < SourceResponseType > sourceResponse =
            searchResponse.getResponse().getResponses().getSourceResponse();
        assertEquals(1, sourceResponse.size());
        /*
        assertTrue(sourceResponse.get(0).getTotal() > 0);
        assertEquals(0, sourceResponse.get(0).getOffset());
        assertEquals("WEB", sourceResponse.get(0).getSource().name());
        List < ResultType > result = sourceResponse.get(0).getResults().getResult();
        assertEquals(1, result.size());
        assertTrue(result.get(0).getTitle().length() > 0);
        assertTrue(result.get(0).getDescription().length() > 0);
        assertTrue(result.get(0).getUrl().length() > 0);
        */
    }
}

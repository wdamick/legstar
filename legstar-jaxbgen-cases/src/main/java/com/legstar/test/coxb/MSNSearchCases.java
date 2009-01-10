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
package com.legstar.test.coxb;


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
public final class MSNSearchCases {

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
        searchRequestType.setQuery("live search");
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
        sourceResponse.setTotal(30);

        ArrayOfResultResultsType results = of.createArrayOfResultResultsType();
        ResultType result = of.createResultType();
        result.setDescription("Study sets and science gifts from Jensan Scientifics."
                + " Your source for fantastic items of Space ... 23"
                + " Legged Starfish measures 5\" across. Great for stirring the"
                + " imagination about what else can be ... ");
        result.setTitle("23 Legged Starfish");
        result.setUrl("http://www.sciencemall-usa.com/23legstar.html");
        results.getResult().add(result);

        sourceResponse.setResults(results);

        sourceResponses.getSourceResponse().add(sourceResponse);

        searchResponses.setResponses(sourceResponses);

        response.setResponse(searchResponses);

        return response;
    }

}

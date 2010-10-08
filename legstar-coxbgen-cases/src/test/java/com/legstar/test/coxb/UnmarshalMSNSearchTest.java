/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.test.coxb;

import java.io.StringWriter;

import junit.framework.TestCase;

import com.legstar.coxb.host.HostData;
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.test.coxb.MSNSearch.SearchResponse;
import com.legstar.test.coxb.MSNSearch.bind.SearchResponseHostToJavaTransformer;
import com.legstar.test.coxb.MSNSearch.bind.SearchResponseHostToJsonTransformer;

/**
 * Unmarshal lsfileae.
 * 
 */
public class UnmarshalMSNSearchTest extends TestCase {

    /**
     * Transform host data and test java data object result.
     * 
     * @throws HostTransformException if transforming fails
     */
    public void testHostToJavaTransformerSearchResponse()
            throws HostTransformException {

        SearchResponseHostToJavaTransformer transformer = new SearchResponseHostToJavaTransformer();
        SearchResponse searchResponse = transformer.transform(
                HostData.toByteArray(MSNSearchCases.getHostBytesHexResponse()));
        MSNSearchCases.checkJavaObjectResponse(searchResponse);
    }

    /**
     * Transform host data and test JSON result.
     * 
     * @throws HostTransformException if transforming fails
     */
    public void testHostToJsonTransformerSearchResponse()
            throws HostTransformException {

        SearchResponseHostToJsonTransformer transformer = new SearchResponseHostToJsonTransformer();
        StringWriter writer = new StringWriter();
        transformer.transform(
                HostData.toByteArray(MSNSearchCases.getHostBytesHexResponse()),
                writer);
        assertEquals(MSNSearchCases.getSearchResponseJson(), writer.toString());
    }
}

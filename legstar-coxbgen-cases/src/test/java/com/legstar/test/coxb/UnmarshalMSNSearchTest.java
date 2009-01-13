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
package com.legstar.test.coxb;



import com.legstar.coxb.host.HostData;
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.test.coxb.MSNSearch.SearchResponse;
import com.legstar.test.coxb.MSNSearch.bind.SearchResponseHostToJavaTransformer;

import junit.framework.TestCase;

/**
 * Unmarshal lsfileae.
 *
 */
public class UnmarshalMSNSearchTest extends TestCase {

    /**
     * Transform host data and test java data object result.
     * @throws HostTransformException if transforming fails
     */
    public void testHostToJavaTransformerSearchResponse() throws HostTransformException {

        SearchResponseHostToJavaTransformer transformer = new SearchResponseHostToJavaTransformer();
        SearchResponse searchResponse = transformer.transform(
                HostData.toByteArray(MSNSearchCases.getHostBytesHexResponse()));
        MSNSearchCases.checkJavaObjectResponse(searchResponse);
    }
    
}

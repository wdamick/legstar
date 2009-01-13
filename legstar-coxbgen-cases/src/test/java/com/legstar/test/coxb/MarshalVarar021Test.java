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
import com.legstar.test.coxb.varar021.SearchGrplst;
import com.legstar.test.coxb.varar021.bind.SearchGrplstJavaToHostTransformer;

import junit.framework.TestCase;

/**
 * Marshal varar021.
 *
 */
public class MarshalVarar021Test extends TestCase {

    /**
     * Marshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testVarar021Empty() throws Exception {

        SearchGrplst searchGrplst = Varar021Cases.getJavaObjectEmpty();
        assertEquals(Varar021Cases.getHostBytesHexEmpty(),
                Util.marshal("varar021", "SearchGrplst", searchGrplst,
                        Varar021Cases.getHostBytesHexEmpty().length() / 2));
    }

    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformerEmpty() throws Exception {

        SearchGrplstJavaToHostTransformer transformer = new SearchGrplstJavaToHostTransformer();
        assertEquals(Varar021Cases.getHostBytesHexEmpty(),
                HostData.toHexString(transformer.transform(Varar021Cases.getJavaObjectEmpty())));
    }

    /**
     * Marshal java data object and test host data result.
     * No IStaticData.
     * @throws Exception if marshaling fails
     */
    public void testVarar021NoIStaticData() throws Exception {

        SearchGrplst searchGrplst = Varar021Cases.getJavaObjectNoIStaticData();
        assertEquals(Varar021Cases.getHostBytesHexNoIStaticData(),
                Util.marshal("varar021", "SearchGrplst", searchGrplst,
                        Varar021Cases.getHostBytesHexNoIStaticData().length() / 2));
    }

    /**
     * Transform java data object and test host data result.
     * No IStaticData.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformerNoIStaticData() throws Exception {

        SearchGrplstJavaToHostTransformer transformer = new SearchGrplstJavaToHostTransformer();
        assertEquals(Varar021Cases.getHostBytesHexNoIStaticData(),
                HostData.toHexString(transformer.transform(Varar021Cases.getJavaObjectNoIStaticData())));
    }

    /**
     * Marshal java data object and test host data result.
     * With IStaticData.
     * @throws Exception if marshaling fails
     */
    public void testVarar021WithIStaticData() throws Exception {

        SearchGrplst searchGrplst = Varar021Cases.getJavaObjectWithIStaticData();
        assertEquals(Varar021Cases.getHostBytesHexWithIStaticData(),
                Util.marshal("varar021", "SearchGrplst", searchGrplst,
                        Varar021Cases.getHostBytesHexWithIStaticData().length() / 2));
    }

    /**
     * Transform java data object and test host data result.
     * With IStaticData.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformerWithIStaticData() throws Exception {

        SearchGrplstJavaToHostTransformer transformer = new SearchGrplstJavaToHostTransformer();
        assertEquals(Varar021Cases.getHostBytesHexWithIStaticData(),
                HostData.toHexString(transformer.transform(Varar021Cases.getJavaObjectWithIStaticData())));
    }
}

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
import com.legstar.test.coxb.varar021.bind.SearchGrplstHostToJavaTransformer;
import com.legstar.test.coxb.varar021.SearchGrplst;

import junit.framework.TestCase;

/**
 * Unmarshal vara021.
 *
 */
public class UnmarshalVarar021Test extends TestCase {

    /**
     * Unmarshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testVarar021Empty() throws Exception {

        SearchGrplst searchGrplst = (SearchGrplst) Util.unmarshal(
                HostData.toByteArray(Varar021Cases.getHostBytesHexEmpty()), "varar021", "SearchGrplst");
        Varar021Cases.checkJavaObjectEmpty(searchGrplst);
    }

    /**
     * Transform host data and test java data object result.
     * Empty case.
     * @throws Exception if transforming fails
     */
    public void testHostToJavaTransformerEmpty() throws Exception {

        SearchGrplstHostToJavaTransformer transformer = new SearchGrplstHostToJavaTransformer();
        SearchGrplst searchGrplst  = transformer.transform(
                HostData.toByteArray(Varar021Cases.getHostBytesHexEmpty()));
        Varar021Cases.checkJavaObjectEmpty(searchGrplst);
    }

    /**
     * Unmarshal host data and test java data object result.
     * NO IStaticData
     * @throws Exception if marshaling fails
     */
    public void testVarar021NoIStaticData() throws Exception {

        SearchGrplst searchGrplst = (SearchGrplst) Util.unmarshal(
                HostData.toByteArray(Varar021Cases.getHostBytesHexNoIStaticData()), "varar021", "SearchGrplst");
        Varar021Cases.checkJavaObjectNoIStaticData(searchGrplst);
    }

    /**
     * Transform host data and test java data object result.
     * NO IStaticData
     * @throws Exception if transforming fails
     */
    public void testHostToJavaTransformerNoIStaticData() throws Exception {

        SearchGrplstHostToJavaTransformer transformer = new SearchGrplstHostToJavaTransformer();
        SearchGrplst searchGrplst  = transformer.transform(
                HostData.toByteArray(Varar021Cases.getHostBytesHexNoIStaticData()));
        Varar021Cases.checkJavaObjectNoIStaticData(searchGrplst);
    }

    /**
     * Unmarshal host data and test java data object result.
     * With IStaticData
     * @throws Exception if marshaling fails
     */
    public void testVarar021WithIStaticData() throws Exception {

        SearchGrplst searchGrplst = (SearchGrplst) Util.unmarshal(
                HostData.toByteArray(Varar021Cases.getHostBytesHexWithIStaticData()), "varar021", "SearchGrplst");
        Varar021Cases.checkJavaObjectWithIStaticData(searchGrplst);
    }

    /**
     * Transform host data and test java data object result.
     * With IStaticData
     * @throws Exception if transforming fails
     */
    public void testHostToJavaTransformerWithIStaticData() throws Exception {

        SearchGrplstHostToJavaTransformer transformer = new SearchGrplstHostToJavaTransformer();
        SearchGrplst searchGrplst  = transformer.transform(
                HostData.toByteArray(Varar021Cases.getHostBytesHexWithIStaticData()));
        Varar021Cases.checkJavaObjectWithIStaticData(searchGrplst);
    }
}

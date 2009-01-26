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

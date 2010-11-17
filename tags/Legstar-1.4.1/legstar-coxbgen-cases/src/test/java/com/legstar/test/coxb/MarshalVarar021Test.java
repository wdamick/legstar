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

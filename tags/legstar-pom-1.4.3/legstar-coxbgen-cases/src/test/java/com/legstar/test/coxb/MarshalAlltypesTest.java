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
import com.legstar.test.coxb.alltypes.Dfhcommarea;
import com.legstar.test.coxb.alltypes.bind.DfhcommareaJavaToHostTransformer;

import junit.framework.TestCase;

/**
 * Marshal alltypes.
 *
 */
public class MarshalAlltypesTest extends TestCase {

    /**
     * Marshal java data object and test host data result.
     * @throws Exception if marshaling fails
     */
    public void testAlltypes() throws Exception {

        Dfhcommarea dfhcommarea = AlltypesCases.getJavaObject();
        assertEquals(AlltypesCases.getHostBytesHex(),
                Util.marshal("alltypes", dfhcommarea, 267));
    }

    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformer() throws Exception {

        DfhcommareaJavaToHostTransformer transformer = new DfhcommareaJavaToHostTransformer();
        assertEquals(AlltypesCases.getHostBytesHex(),
                HostData.toHexString(transformer.transform(AlltypesCases.getJavaObject())));
    }
}

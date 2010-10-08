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
import com.legstar.test.coxb.arraygrp.bind.DfhcommareaTransformers;
import com.legstar.test.coxb.arraygrp.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Marshal arraygrp.
 *
 */
public class MarshalArraygrpTest extends TestCase {

    /**
     * Transform java data object and test host data result.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformer() throws Exception {

        Dfhcommarea dfhcommarea = new Dfhcommarea();
        DfhcommareaTransformers transformers = new DfhcommareaTransformers();
        assertEquals("40404040404040404040f0f0f0f0f0f0f0f0f0"
                + "40404040404040404040f0f0f0f0f0f0f0f0f0"
                + "40404040404040404040f0f0f0f0f0f0f0f0f0"
                + "40404040404040404040f0f0f0f0f0f0f0f0f0",
                HostData.toHexString(transformers.toHost(dfhcommarea)));
    }
}

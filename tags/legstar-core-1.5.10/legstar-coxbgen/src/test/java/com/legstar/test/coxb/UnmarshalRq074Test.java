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

import junit.framework.TestCase;

import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.rq074.Dfhcommarea;

/**
 * Unmarshal Rq074 (levels 88).
 * 
 */
public class UnmarshalRq074Test extends TestCase {

    /**
     * Unmarshal host data and test java data object result.
     * 
     * @throws Exception if marshaling fails
     */
    public void testRq014() throws Exception {

        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(
                HostData.toByteArray("c3d9c50000"), "rq074");

        assertEquals("VALUE_CRE", dfhcommarea.getRq074Crud().toString());
        assertEquals("VALUE_0", dfhcommarea.getRq074Bool().toString());
    }

}

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

import com.legstar.test.coxb.rq074.Dfhcommarea;
import com.legstar.test.coxb.rq074.Dfhcommarea.Rq074Bool;
import com.legstar.test.coxb.rq074.Dfhcommarea.Rq074Crud;

/**
 * Marshal Rq074 (levels 88).
 * 
 */
public class MarshalRq074Test extends TestCase {

    /** The annotated XSD file name. */
    private static final String SCHEMA_NAME = "rq074";

    /**
     * Marshal java data object and test host data result.
     * 
     * @throws Exception if marshaling fails
     */
    public void testRq014() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        Dfhcommarea dfhcommarea = new Dfhcommarea();
        dfhcommarea.setRq074Bool(Rq074Bool.VALUE_0);
        dfhcommarea.setRq074Crud(Rq074Crud.VALUE_CRE);
        assertEquals("c3d9c50000", Util.marshal(SCHEMA_NAME, dfhcommarea, 5));
    }

}

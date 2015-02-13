/*******************************************************************************
 * Copyright (c) 2015 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.impl.reflect;

import com.legstar.test.coxb.rq074.Dfhcommarea;

/**
 * Test Rq074 level 88 enumeration values (string and numeric).
 * 
 */
public class UnmarshalRq074Test extends AbstractTestUnmarshal {

    /**
     * Case of numeric value zero.
     */
    public void testRq014Value0() {
        Dfhcommarea dfhcommarea = (Dfhcommarea) convert(
                new com.legstar.test.coxb.rq074.ObjectFactory(), "c3d9c50000",
                new Dfhcommarea());
        assertEquals("VALUE_CRE", dfhcommarea.getRq074Crud().toString());
        assertEquals("VALUE_0", dfhcommarea.getRq074Bool().toString());
    }

    /**
     * Case of numeric value 1.
     */
    public void testRq014Value1() {
        Dfhcommarea dfhcommarea = (Dfhcommarea) convert(
                new com.legstar.test.coxb.rq074.ObjectFactory(), "e4d7c40001",
                new Dfhcommarea());
        assertEquals("VALUE_UPD", dfhcommarea.getRq074Crud().toString());
        assertEquals("VALUE_1", dfhcommarea.getRq074Bool().toString());
    }
}

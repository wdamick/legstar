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
package com.legstar.coxb.impl.reflect;

import com.legstar.test.coxb.rq074.Dfhcommarea;
import com.legstar.test.coxb.rq074.Dfhcommarea.Rq074Bool;
import com.legstar.test.coxb.rq074.Dfhcommarea.Rq074Crud;

/**
 * Marshal Rq074 level 88 enumeration values (string and numeric).
 * 
 */
public class MarshalRq074Test extends AbstractTestMarshal {

    /**
     * Case of numeric value zero.
     */
    public void testRq014Value0() {
        Dfhcommarea dfhcommarea = new Dfhcommarea();
        dfhcommarea.setRq074Bool(Rq074Bool.VALUE_0);
        dfhcommarea.setRq074Crud(Rq074Crud.VALUE_CRE);
        convertAndCheck(new com.legstar.test.coxb.rq074.ObjectFactory(),
                dfhcommarea, "c3d9c50000");
    }

    /**
     * Case of numeric value 1.
     */
    public void testRq014Value1() {
        Dfhcommarea dfhcommarea = new Dfhcommarea();
        dfhcommarea.setRq074Bool(Rq074Bool.VALUE_1);
        dfhcommarea.setRq074Crud(Rq074Crud.VALUE_UPD);
        convertAndCheck(new com.legstar.test.coxb.rq074.ObjectFactory(),
                dfhcommarea, "e4d7c40001");
    }
}

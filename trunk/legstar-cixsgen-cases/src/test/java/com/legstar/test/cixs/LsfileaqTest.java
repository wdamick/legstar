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
package com.legstar.test.cixs;

import com.legstar.test.cixs.lsfileaq.LsfileaqFault;
import com.legstar.test.cixs.lsfileaq.LsfileaqImpl;
import com.legstar.test.coxb.LsfileaqCases;
import com.legstar.test.coxb.lsfileaq.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Test LSfileaq adapter.
 *
 */
public class LsfileaqTest extends TestCase {
    
    /**
     * Direct host invoke.
     * @throws LsfileaqFault if test fails
     */
    public void testLsfileaqRequest100() throws LsfileaqFault {
        LsfileaqImpl port = new LsfileaqImpl();
        Dfhcommarea request = LsfileaqCases.getJavaObjectRequest5();
        Dfhcommarea reply = port.lsfileaq(request, null);
        LsfileaqCases.checkJavaObjectReply5(reply);
    }
    
}

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

import com.legstar.test.cixs.lsfileae.LsfileaeFault;
import com.legstar.test.cixs.lsfileae.LsfileaeImpl;
import com.legstar.test.coxb.LsfileaeCases;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Test LSFILEAE adapter.
 *
 */
public class LsfileaeITCase extends TestCase {
    
    /**
     * Direct host invoke.
     * @throws LsfileaeFault if test fails
     */
    public void testLsfileaeRequest100() throws LsfileaeFault {
        LsfileaeImpl port = new LsfileaeImpl();
        Dfhcommarea request = LsfileaeCases.getJavaObjectRequest100();
        Dfhcommarea reply = port.lsfileae(request, null);
        LsfileaeCases.checkJavaObjectReply100(reply);
    }

}

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
package com.legstar.test.cixs;

import com.legstar.test.cixs.lsfileal.LsfilealFault;
import com.legstar.test.cixs.lsfileal.LsfilealImpl;
import com.legstar.test.coxb.LsfilealCases;
import com.legstar.test.coxb.lsfileal.RequestParms;
import com.legstar.test.coxb.lsfileal.ReplyData;

import junit.framework.TestCase;

/**
 * Test LSFILEAL adapter.
 *
 */
public class LsfilealTest extends TestCase {
    
    /**
      * Direct host invoke.
     * @throws LsfilealFault if test fails
     */
    public void testLsfilealRequestSStar() throws LsfilealFault {
        LsfilealImpl port = new LsfilealImpl();
        RequestParms request = LsfilealCases.getJavaObjectRequestSStar();
        ReplyData reply = port.lsfileal(request, null);
        LsfilealCases.checkJavaObjectReeplySStar(reply);
    }

}

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

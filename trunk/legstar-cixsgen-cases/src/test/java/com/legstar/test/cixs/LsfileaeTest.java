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
public class LsfileaeTest extends TestCase {
    
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

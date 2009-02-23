package com.legstar.coxb.transform;

import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.LsfileaeCases;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;


/**
 * This class is useful for performance testing with JMeter.
 *
 */
public class LsfileaeMeteringTest extends AbstractTestTransformers {
    
    /** A byte array holding raw mainframe data. */
    private static final byte[] LSFILEAE_HOST_BYTES =
        HostData.toByteArray(LsfileaeCases.getHostBytesHex());
    
    /**
     * LSFILEAE from Java to Host.
     */
    public void testJavaToHost() {
        try {
            LsfileaeTransformers transformers = new LsfileaeTransformers();
            Dfhcommarea dfhcommarea = (Dfhcommarea) transformers.toJava(
                    LSFILEAE_HOST_BYTES, STRING_US_CHARSET);
            LsfileaeCases.checkJavaObject(dfhcommarea);
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }

    /**
     * LSFILEAE from Host to Java.
     */
    public void testHostToJava() {
        try {
            LsfileaeTransformers transformers = new LsfileaeTransformers();
            byte[] hostBytes = transformers.toHost(
                    LsfileaeCases.getJavaObject(), STRING_US_CHARSET);
            assertEquals(LsfileaeCases.getHostBytesHex(), HostData.toHexString(hostBytes));
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }
}

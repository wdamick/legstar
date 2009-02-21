package com.legstar.coxb.transform;

import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;


/**
 * This class is useful for performance testing with JMeter.
 *
 */
public class MeteringTest extends AbstractTestTransformers {
    
    /** A byte array holding raw mainframe data. */
    private static final byte[] LSFILEAE_HOST_BYTES =
        HostData.toByteArray(RAW_LSFILEAE_DATA_IBM01147);
    
    /**
     * LSFILEAE from Java to Host.
     */
    public void testJavaToHostLsfileae() {
        try {
            LsfileaeTransformers transformers = new LsfileaeTransformers();
            Dfhcommarea dfhcommarea = (Dfhcommarea) transformers.toJava(
                    LSFILEAE_HOST_BYTES, STRING_FRENCH_CHARSET);
            checkLsfileaeIBM01147(dfhcommarea);
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }

    /**
     * LSFILEAE from Host to Java.
     */
    public void testHostToJavaLsfileae() {
        try {
            LsfileaeTransformers transformers = new LsfileaeTransformers();
            byte[] hostBytes = transformers.toHost(
                    getLsfileaeObject(), STRING_FRENCH_CHARSET);
            assertEquals(RAW_LSFILEAE_DATA_IBM01147, HostData.toHexString(hostBytes));
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }
}

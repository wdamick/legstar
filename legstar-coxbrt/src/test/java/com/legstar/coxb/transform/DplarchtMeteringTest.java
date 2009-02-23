package com.legstar.coxb.transform;

import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.DplarchtCases;
import com.legstar.test.coxb.dplarcht.Dfhcommarea;


/**
 * This class is useful for performance testing with JMeter.
 * It simulates 3 different payload sizes by varying the number of items in
 * a variable size array.
 *
 */
public class DplarchtMeteringTest extends AbstractTestTransformers {
    
    /** A byte array holding raw mainframe data. Small volume. */
    private static final byte[] DPLARCHT_HOST_BYTES_SMALL_VOLUME =
        HostData.toByteArray(DplarchtCases.getHostBytesHexFiles(0));
    
    /** A byte array holding raw mainframe data. Medium volume. */
    private static final byte[] DPLARCHT_HOST_BYTES_MEDIUM_VOLUME =
        HostData.toByteArray(DplarchtCases.getHostBytesHexFiles(100));
    
    /** A byte array holding raw mainframe data. Large volume. */
    private static final byte[] DPLARCHT_HOST_BYTES_LARGE_VOLUME =
        HostData.toByteArray(DplarchtCases.getHostBytesHexFiles(500));
    
    /**
     * DPLARCHT from Java to Host Small payload.
     */
    public void testJavaToHostSmallVolume() {
        try {
            DplarchtTransformers transformers = new DplarchtTransformers();
            Dfhcommarea dfhcommarea = (Dfhcommarea) transformers.toJava(
                    DPLARCHT_HOST_BYTES_SMALL_VOLUME, STRING_US_CHARSET);
            DplarchtCases.checkJavaObjectFiles(0, dfhcommarea);
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }

    /**
     * DPLARCHT from Java to Host Medium payload.
     */
    public void testJavaToHostMediumVolume() {
        try {
            DplarchtTransformers transformers = new DplarchtTransformers();
            Dfhcommarea dfhcommarea = (Dfhcommarea) transformers.toJava(
                    DPLARCHT_HOST_BYTES_MEDIUM_VOLUME, STRING_US_CHARSET);
            DplarchtCases.checkJavaObjectFiles(100, dfhcommarea);
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }

    /**
     * DPLARCHT from Java to Host Large volume.
     */
    public void testJavaToHostLargeVolume() {
        try {
            DplarchtTransformers transformers = new DplarchtTransformers();
            Dfhcommarea dfhcommarea = (Dfhcommarea) transformers.toJava(
                    DPLARCHT_HOST_BYTES_LARGE_VOLUME, STRING_US_CHARSET);
            DplarchtCases.checkJavaObjectFiles(500, dfhcommarea);
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }


    /**
     * DPLARCHT from Host to Java Small payload.
     */
    public void testHostToJavaSmallVolume() {
        try {
            DplarchtTransformers transformers = new DplarchtTransformers();
            byte[] hostBytes = transformers.toHost(
                    DplarchtCases.getJavaObjectFiles(0), STRING_US_CHARSET);
            assertEquals(25, hostBytes.length);
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }

    /**
     * DPLARCHT from Host to Java Medium payload.
     */
    public void testHostToJavaMediumVolume() {
        try {
            DplarchtTransformers transformers = new DplarchtTransformers();
            byte[] hostBytes = transformers.toHost(
                    DplarchtCases.getJavaObjectFiles(100), STRING_US_CHARSET);
            assertEquals(6425, hostBytes.length);
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }

    /**
     * DPLARCHT from Host to Java Large payload.
     */
    public void testHostToJavaLargeVolume() {
        try {
            DplarchtTransformers transformers = new DplarchtTransformers();
            byte[] hostBytes = transformers.toHost(
                    DplarchtCases.getJavaObjectFiles(500), STRING_US_CHARSET);
            assertEquals(32025, hostBytes.length);
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }
}

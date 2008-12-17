package com.legstar.host.invoke;

import com.legstar.test.coxb.lsfileae.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Generic helper code for unit tests.
 *
 */
public class AbstractTestInvokers extends TestCase {

    /** A valid invoker configuration file. */
    public static final String CONFIG_FILE = "config0.xml";
    
    /** Mainframe user ID. */
    public static final String HOST_USERID = "P390";

    /** Mainframe password. */
    public static final String HOST_PASSWORD = "STREAM2";
    
    /** A sample host byte array request for LSFILEAE. */
    public static final String LSFILEAE_BYTES_REQUEST =
        /*  0 0 0 1 0 0 */
          "f0f0f0f1f0f0"
        /*                                          */
        + "4040404040404040404040404040404040404040"
        /*                                          */
        + "4040404040404040404040404040404040404040"
        /*                  */
        + "4040404040404040"
        /*                  */
        + "4040404040404040"
        /*  0 0 0 0 0 . 0 0 */
        + "f0f0f0f0f00bf0f0"
        /*                     */
        + "404040404040404040";

    /** A sample host byte array reply for LSFILEAE. */
    public static final String LSFILEAE_BYTES_REPLY =
        /*  0 0 0 1 0 0 */
          "f0f0f0f1f0f0"
        /*  S .  D .   B O R M A N       */
        + "e24b40c44b40c2d6d9d4c1d54040404040404040"
        /*  L A B A S   S T R E E T                 */
        + "e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040"
        /*  3 2 1 5 6 7 7 8 */
        + "f3f2f1f5f6f7f7f8"
        /*  2 6   1 1   8 1  */
        + "f2f640f1f140f8f1"
        /*  $ 0 1 0 0 . 1 1  */
        + "5bf0f1f0f04bf1f1"
        /*  * * * * * * * * *  */
        + "5c5c5c5c5c5c5c5c5c";
    
    /** Sample QueryData container content. */
    public static final String LSFILEAC_QUERYDATA_BYTES =
        /*  S *                                    */
          "e25c404040404040404040404040404040404040"
        /*  *                                      */
        + "5c40404040404040404040404040404040404040"
        /*  *               */
        + "5c40404040404040";
    
    /** Sample QueryLimit container content. */
    public static final String LSFILEAC_QUERYLIMIT_BYTES =
        /*  5000 *                                  */
          "000050000f"
        /*  100 *                                  */
        + "000001000f";
    
    
    /** Sample QueryReply container content. */
    public static final String LSFILEAC_QUERYREPLY_BYTES =
        /*        5*/  
          "000000005f"
        + "f0f0f0f1f0f0"
        + "e24b40c44b40c2d6d9d4c1d54040404040404040"
        + "e2e4d9d9c5e86b40c5d5c7d3c1d5c44040404040"
        + "f3f2f1f5f6f7f7f8"
        + "f2f640f1f140f8f1"
        + "5bf0f1f0f04bf1f1"
        + "5c5c5c5c5c5c5c5c5c"
        
        + "f0f0f0f7f6f2"
        + "e2e4e2c1d540d4c1d3c1c9d2c140404040404040"
        + "e2c1d540d1d6e2c56bc3c1d3c9c6d6d9d5c9c140"
        + "f2f2f3f1f2f1f2f1"
        + "f0f140f0f640f7f4"
        + "5bf0f0f0f04bf0f0"
        + "5c5c5c5c5c5c5c5c5c"
        
        + "f0f0f6f0f1f6"
        + "e2c9d940d4c9c3c8c1c5d340d9d6c2c5d9e3e240"
        + "d5c5e640c4c5d3c8c96b40c9d5c4c9c140404040"
        + "f7f0f3f3f1f2f1f1"
        + "f2f140f0f540f7f4"
        + "5bf0f0f0f94bf8f8"
        + "5c5c5c5c5c5c5c5c5c"
        
        + "f2f0f0f0f0f0"
        + "e24b40d74b40d9e4e2e2c5d3d340404040404040"
        + "c7d3c1e2c7d6e66b4040e2c3d6e3d3c1d5c44040"
        + "f6f3f7f3f8f2f9f0"
        + "f2f640f1f140f8f1"
        + "5bf0f0f2f04bf0f0"
        + "5c5c5c5c5c5c5c5c5c"
        
        + "f5f5f5f5f5f5"
        + "e24bd14b40d3c1e9c5d5c2e84040404040404040"
        + "d2c9d5c7e2e3d6d56b40d54be84b404040404040"
        + "f3f9f9f4f4f4f2f0"
        + "f2f640f1f140f8f1"
        + "5bf0f0f0f54bf0f0"
        + "5c5c5c5c5c5c5c5c5c";
    
    /** Sample ReplyStatus container content. */
    public static final String LSFILEAC_REPLYSTATUS_BYTES =
        "0000"
        + "f0f07af0f07af0f0"
        + "000000044f"
        + "00000000"
        + "00000000"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040";
    
    /** Sample ReplyStatus container content when there is no match. */
    public static final String LSFILEAC_REPLYSTATUS_NOMATCH_BYTES =
        "0000"
        + "f0f07af0f07af0f0"
        + "000000044f"
        + "00000000"
        + "00000000"
        + "d5d640c3e4e2e3d6d4c5d940e2c1e3c9e2c6c9c5e240e8d6e4d940d8e4c5d9e8"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040404040404040404040404040";
    
    /** Check the values returned from LSFILAE after they were transformed to Java*/
    public void checkLsfileaeJavaReply(final Dfhcommarea dfhcommarea) {
        assertEquals(100, dfhcommarea.getComNumber());
        assertEquals("$0100.11", dfhcommarea.getComAmount());
        assertEquals("*********", dfhcommarea.getComComment());
        assertEquals("26 11 81", dfhcommarea.getComDate());
        assertEquals("SURREY, ENGLAND", dfhcommarea.getComPersonal().getComAddress());
        assertEquals("S. D. BORMAN", dfhcommarea.getComPersonal().getComName());
        assertEquals("32156778", dfhcommarea.getComPersonal().getComPhone());
    }
}

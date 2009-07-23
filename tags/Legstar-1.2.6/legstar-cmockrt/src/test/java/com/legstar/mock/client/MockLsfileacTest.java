package com.legstar.mock.client;

import com.legstar.coxb.host.HostData;
import com.legstar.messaging.ContainerPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.RequestException;

import junit.framework.TestCase;

/**
 * Test MockLsfileac.
 *
 */
public class MockLsfileacTest extends TestCase {
    
    /** A sample LSFILEAC query data.*/
    public static final String LSFILEAC_QUERY_DATA =
       /* S * */
          "e25c404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040";
    
    /** A sample LSFILEAC query limit.*/
    public static final String LSFILEAC_QUERY_LIMIT =
          "000000055f"
        + "000000055f";

    /** A sample LSFILEAC reply status.*/
    public static final String LSFILEAC_REPLY_STATUS =
        "0000"
        + "f0f07af0f07af0f0"
        + "000000043f"
        + "00000000"
        + "00000000"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040404040404040404040404040"
        + "4040404040404040";

        /** A sample LSFILEAC reply data.*/
    public static final String LSFILEAC_REPLY_DATA =
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

    /**
     * Test return value.
     */
    public void testMockLsfileac() {
        try {
            LegStarMessage requestMessage = new LegStarMessage();
            requestMessage.addDataPart(new ContainerPart(
                    "QueryData", HostData.toByteArray(LSFILEAC_QUERY_DATA)));
            requestMessage.addDataPart(new ContainerPart(
                    "QueryLimit", HostData.toByteArray(LSFILEAC_QUERY_LIMIT)));
            LegStarMessage replyMessage = MockLsfileac.getResponse(requestMessage);
            assertEquals(LSFILEAC_REPLY_STATUS,
                    HostData.toHexString(replyMessage.getDataParts().get(0).getContent()));
            assertEquals(LSFILEAC_REPLY_DATA,
                    HostData.toHexString(replyMessage.getDataParts().get(1).getContent()));
        } catch (HeaderPartException e) {
            fail(e.toString());
        } catch (RequestException e) {
            fail(e.toString());
        }
    }

}

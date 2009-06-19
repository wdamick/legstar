package com.legstar.http.client;

import junit.framework.TestCase;

import org.apache.commons.configuration.ConfigurationException;

import com.legstar.config.Config;
import com.legstar.coxb.host.HostData;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;
import com.legstar.test.client.AbstractConnectionTester;
import com.legstar.test.coxb.LsfileaeCases;

/**
 * Load test in a configuration that is close to the way invoker works.
 * Invokers recreate a connection on each request without pooling.
 *
 */
public class CicsTs31InvokeMeteringTest extends TestCase {

    /** The endpoint (one per test).*/
    private CicsHttpEndpoint _endpoint;
    
    /** The endpoint name.*/
    private String _endpointName;
    
    /**
     * Construct.
     * @throws ConfigurationException if endpoint cannot be created
     */
    public CicsTs31InvokeMeteringTest() throws ConfigurationException {
        _endpointName = "CICSTS31";
        _endpoint = new CicsHttpEndpoint(
                Config.loadEndpointConfiguration("config.xml", _endpointName));
        _endpoint.setHostTraceMode(false);
    }

    /**
     * Perform a round trip.
     */
    public void testLsfileae() {
        try {
            CicsHttp connection = new CicsHttp(
                    _endpointName, _endpoint,
                    AbstractHttpConnectionTester.DEFAULT_CONNECT_TIMEOUT_MSEC,
                    AbstractHttpConnectionTester.DEFAULT_READ_TIMEOUT_MSEC);
            connection.connect(AbstractConnectionTester.HOST_USERID);
            LegStarRequest request = AbstractConnectionTester.getLsfileaeRequest100(
                    new LegStarAddress(_endpointName));
            connection.sendRequest(request);
            connection.recvResponse(request);
            connection.close();
            assertEquals(1, request.getResponseMessage().getHeaderPart().getDataPartsNumber());
            assertEquals(LsfileaeCases.getHostBytesHexReply100(),
                    HostData.toHexString(request.getResponseMessage().getDataParts().get(0).getContent()));
        } catch (RequestException e) {
            e.printStackTrace();
            fail(e.toString());
        } catch (ConnectionException e) {
            e.printStackTrace();
            fail(e.toString());
        }
    }
}

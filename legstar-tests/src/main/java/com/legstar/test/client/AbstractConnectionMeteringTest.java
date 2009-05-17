package com.legstar.test.client;

import com.legstar.coxb.host.HostData;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarConnection;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;
import com.legstar.test.coxb.LsfileaeCases;

import junit.framework.TestCase;

/**
 * Common code for metering tests without pooling engine.
 *
 */
public abstract class AbstractConnectionMeteringTest extends TestCase {
    
    /** A connection without pooling. */
    private LegStarConnection _connection;
    
    /** Connection configuration file name. */
    private String _configFileName;
    
    /** Target endpoint name. */
    private String _endpointName;
    
    /**
     * Create resources at construction time rather than setUp so that it is done
     * only once when this test is repeated by JMeter.
     * @param configFileName the connection configuration file name
     * @param endpointName the target mainframe endpoint name
     * @throws ConnectionException if connection cannot be created
     */
    public AbstractConnectionMeteringTest(
            final String configFileName, final String endpointName) throws ConnectionException {
        _configFileName = configFileName;
        _endpointName = endpointName;
        _connection = createConnection(configFileName, endpointName);
    }
    
    /**
     * Create a new connection for a given endpoint name.
     * @param configFileName the connection configuration file name
     * @param endpointName the endpoint name for which a connection is needed
     * @throws ConnectionException is connection cannot be created
     * @return a connection
     */
    public abstract LegStarConnection createConnection(
            final String configFileName,
            final String endpointName) throws ConnectionException;

    /**
     * Perform a round trip.
     */
    public void testLsfileae() {
        try {
            _connection.connect(AbstractConnectionTester.HOST_USERID);
            LegStarRequest request = AbstractConnectionTester.getLsfileaeRequest100(
                    new LegStarAddress(_endpointName));
            _connection.sendRequest(request);
            _connection.recvResponse(request);
            _connection.close();
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
    
    /**
     * @return the connection configuration file name
     */
    public String getConfigFileName() {
        return _configFileName;
    }

    /**
     * @return the target endpoint name
     */
    public String getEndpointName() {
        return _endpointName;
    }
    
    /**
     * @return a connection without pooling
     */
    public LegStarConnection getConnection() {
        return _connection;
    }
}

package com.legstar.test.connection.client;

import com.legstar.coxb.host.HostData;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.HostEndpoint;
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
    
    /** The target host endpoint.*/
    private HostEndpoint _endpoint;

    /** A connection without pooling. */
    private LegStarConnection _connection;
    
    
    /** {@inheritDoc} */
    public void setUp() throws Exception {
        _endpoint = createEndpoint();
        _connection = createConnection();
    }
    
    /**
     * Create a new connection for a given endpoint name.
     * @throws ConnectionException is connection cannot be created
     * @return a connection
     */
    public abstract LegStarConnection createConnection() throws ConnectionException;
    
    /**
     * @return the target endpoint name
     */
    public abstract HostEndpoint createEndpoint();

    /**
     * Perform a round trip.
     */
    public void testLsfileae() {
        try {
            _connection.connect(AbstractConnectionTester.HOST_PASSWORD);
            LegStarRequest request = AbstractConnectionTester.getLsfileaeRequest100(
                    new LegStarAddress(getEndpoint().getName()));
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
     * @return the target host endpoint
     */
    public HostEndpoint getEndpoint() {
        return _endpoint;
    }

    /**
     * @return a connection without pooling
     */
    public LegStarConnection getConnection() {
        return _connection;
    }
}

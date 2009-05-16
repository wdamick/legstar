package com.legstar.mq.client;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.config.Config;
import com.legstar.coxb.host.HostData;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;
import com.legstar.test.coxb.LsfileaeCases;

import junit.framework.TestCase;

/**
 * Common code for metering tests without pooling engine.
 *
 */
public abstract class AbstractMeteringTest extends TestCase {
    
    /** A connection without pooling. */
    private AbstractCicsMQ _mqConnection;
    
    /** Target endpoint name. */
    private String _endpointName;
    
    /** Configuration file.*/
    public static final String CONFIG_FILE = "config.xml";

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Create resources at construction time rather than setUp so that it is done
     * only once when this test is repeated by JMeter.
     * @param endpointName the target mainframe endpoint name
     */
    public AbstractMeteringTest(final String endpointName) {
        _endpointName = endpointName;
        _mqConnection = createConnection(endpointName);
    }
    
     /**
     * Create a new connection for a given endpoint name.
     * @param endpointName the endpoint for which a connection is needed
     * @return a connection
     */
    private AbstractCicsMQ createConnection(final String endpointName) {
        try {
            _log.info("creating connection for endpoint " + endpointName);
            CicsMQEndpoint legstarEndpoint = new CicsMQEndpoint(
                    Config.loadEndpointConfiguration(CONFIG_FILE, endpointName));
            legstarEndpoint.setHostTraceMode(false);
            AbstractCicsMQ connection = createConnection(endpointName, legstarEndpoint);
            connection.setConnectTimeout(2000);
            return connection;
        } catch (ConfigurationException e) {
            e.printStackTrace();
            throw new IllegalArgumentException(e);
        } catch (CicsMQConnectionException e) {
            throw new IllegalArgumentException(e);
        }
    }
    
    /**
     * Create a new connection for a given endpoint name.
     * @param endpointName the endpoint name for which a connection is needed
     * @param legstarEndpoint the endpoint for which a connection is needed
     * @throws CicsMQConnectionException is connection cannot be created
     * @return a connection
     */
    public abstract AbstractCicsMQ createConnection(
            final String endpointName,
            CicsMQEndpoint legstarEndpoint) throws CicsMQConnectionException;

    /**
     * Perform a round trip.
     */
    public void testLsfileae() {
        try {
            _mqConnection.connect(AbstractTester.HOST_USERID);
            LegStarRequest request = AbstractTester.getLsfileaeRequest100(
                    new LegStarAddress(_endpointName));
            _mqConnection.sendRequest(request);
            _mqConnection.recvResponse(request);
            _mqConnection.close();
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

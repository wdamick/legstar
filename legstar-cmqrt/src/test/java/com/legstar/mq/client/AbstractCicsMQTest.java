/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.mq.client;

import javax.jms.BytesMessage;
import javax.jms.Message;

import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.HostReceiveException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarRequest;
import com.legstar.messaging.RequestException;

/**
 * Tests for methods in AbstractCicsMQ.
 * 
 */
public class AbstractCicsMQTest extends AbstractMQConnectionTester {

    /** {@inheritDoc} */
    public void setUp() throws Exception {
        super.setUp("CICSTS23-LSMSG");
        getEndpoint().setConnectTimeout(2000);
    }

    /**
     * Test connect/close.
     */
    public void testConnectClose() {
        try {
            CicsMQMock cicsMQ = new CicsMQMock("testInstantiation",
                    getEndpoint());
            cicsMQ.connect("tiramisu");
            cicsMQ.close();
        } catch (ConnectionException e) {
            fail(e.getMessage());
        } catch (RequestException e) {
            fail(e.getMessage());
        }
    }

    /**
     * Try to connect to a host not running WMQ.
     */
    public void testConnectWrongHost() {
        try {
            getEndpoint().setJndiConnectionFactoryName(
                    "WrongHostNameConnectionFactory");
            CicsMQMock cicsMQ = new CicsMQMock("testInstantiation",
                    getEndpoint());
            cicsMQ.connect("tiramisu");
            fail("testConnectFailure");
        } catch (ConnectionException e) {
            assertTrue(e.getMessage().contains(
                    "javax.jms.JMSException: MQJMS2005"));
        }
    }

    /**
     * Try to connect to a host using the wrong port.
     */
    public void testConnectWrongPort() {
        try {
            getEndpoint().setJndiConnectionFactoryName(
                    "WrongHostPortConnectionFactory");
            CicsMQMock cicsMQ = new CicsMQMock("testInstantiation",
                    getEndpoint());
            cicsMQ.connect("tiramisu");
            fail("testConnectFailure");
        } catch (ConnectionException e) {
            assertTrue(e.getMessage().contains(
                    "javax.jms.JMSException: MQJMS2005"));
        }
    }

    /**
     * Try to connect twice.
     */
    public void testConnectReuse() {
        try {
            CicsMQMock cicsMQ = new CicsMQMock("testInstantiation",
                    getEndpoint());
            cicsMQ.connectReuse("tiramisu");
            /* We should have a valid JMS objects */
            assertNotNull(cicsMQ.getJmsConnection());
            assertNotNull(cicsMQ.getJmsQueueSession());
            assertNotNull(cicsMQ.getJmsRequestQueue());
            assertNotNull(cicsMQ.getJmsReplyQueue());
            /* This should be accepted */
            cicsMQ.connectReuse("tiramisu");
            assertNotNull(cicsMQ.getJmsConnection());
            assertNotNull(cicsMQ.getJmsQueueSession());
            assertNotNull(cicsMQ.getJmsRequestQueue());
            assertNotNull(cicsMQ.getJmsReplyQueue());
            cicsMQ.close();
            assertNull(cicsMQ.getJmsConnection());
            assertNull(cicsMQ.getJmsQueueSession());
            assertNull(cicsMQ.getJmsRequestQueue());
            assertNull(cicsMQ.getJmsReplyQueue());
        } catch (ConnectionException e) {
            fail(e.getMessage());
        } catch (RequestException e) {
            fail(e.getMessage());
        }
    }

    /**
     * A Mock MQ connection.
     * 
     */
    public class CicsMQMock extends AbstractCicsMQ {

        public CicsMQMock(String connectionID, CicsMQEndpoint cicsMQEndpoint)
                throws CicsMQConnectionException {
            super(connectionID, cicsMQEndpoint);
        }

        @Override
        public Message createRequestMessage(LegStarRequest request)
                throws RequestException {
            return null;
        }

        @Override
        public LegStarMessage createReplyMessage(BytesMessage jmsMessage,
                int dataLength) throws HostReceiveException {
            return null;
        }

    }
}

package com.legstar.host.invoke;

import java.util.Map;

import org.apache.commons.configuration.HierarchicalConfiguration;

import com.legstar.config.Config;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.host.HostData;
import com.legstar.host.AbstractTester;
import com.legstar.host.access.DirectHostAccessStrategy;
import com.legstar.host.access.HostAccessStrategy;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarHeaderPart;
import com.legstar.messaging.LegStarMessage;
import com.legstar.test.coxb.LsfileaeCases;

/**
 * Test the generic Invoker.
 */
public class AbstractInvokerTest extends AbstractTester {
    
    
    /**
     * Test invoke through abstract invoker.
     * @throws Exception if invoke fails
     */
    public void testInvoke() throws Exception {
        LegStarAddress address = new LegStarAddress("TheMainframe");
        HierarchicalConfiguration generalConfig = Config.loadGeneralConfig(CONFIG_FILE);
        HierarchicalConfiguration endpointConfig = Config.loadAddressConfiguration(
                generalConfig, address);
        HostAccessStrategy hostAccessStrategy = new DirectHostAccessStrategy(endpointConfig);
        CicsProgram hostProgram = new CicsProgram("lsfileae.properties");

        AbstractInvokerImpl invoker = new AbstractInvokerImpl(hostAccessStrategy, address, hostProgram);
        byte[] responseBytes = invoker.invoke("lsfileae",
                HostData.toByteArray(LsfileaeCases.getHostBytesHexRequest100()));
        assertTrue(responseBytes !=  null);
        assertEquals(LsfileaeCases.getHostBytesHexReply100(), HostData.toHexString(responseBytes));
    }
    
    /**
     * A mock implementation of the AbstractInvoker.
     *
     */
    public final class AbstractInvokerImpl extends AbstractInvoker {

        /**
         * Constructor.
         * @param hostAccessStrategy the host access strategy
         * @param completeAddress the completed address
         * @param hostProgram the host program
         * @throws HostInvokerException if construction fails
         */
        public AbstractInvokerImpl(final HostAccessStrategy hostAccessStrategy,
                final LegStarAddress completeAddress, final CicsProgram hostProgram)
                throws HostInvokerException {
            super(hostAccessStrategy, completeAddress, hostProgram);
        }

        /** {@inheritDoc} */
        public void invoke(final String requestID, final ICobolComplexBinding ccbin,
                final ICobolComplexBinding ccbout) throws HostInvokerException {
        }

        /** {@inheritDoc} */
        public void invoke(final String requestID,
                final Map < String, ICobolComplexBinding > inParts,
                final Map < String, ICobolComplexBinding > outParts)
                throws HostInvokerException {
        }

        /** {@inheritDoc} */
        public byte[] invoke(final String requestID, final byte[] requestBytes)
                throws HostInvokerException {
            try {
                LegStarMessage requestMessage = new LegStarMessage();
                requestMessage.setHeaderPart(new LegStarHeaderPart(getProgramAttr().getProgramAttrMap(), 0));
                requestMessage.addDataPart(new CommareaPart(requestBytes));

                LegStarMessage responseMessage = invoke(requestID, requestMessage);

                if (responseMessage == null) {
                    return null;
                }
                if (responseMessage.getDataParts().size() == 0) {
                    return new byte[0];
                }
                if (responseMessage.getDataParts().size() > 1) {
                    throw new HostInvokerException("Unexpected number of parts "
                            + responseMessage.getDataParts().size()
                            + " in the host response");
                }
                return responseMessage.getDataParts().get(0).getContent();
            } catch (HeaderPartException e) {
                throw new HostInvokerException(e);
            }
        }

        /** {@inheritDoc} */
        public Map < String, byte[] > invoke(final String requestID,
                final Map < String, byte[] > requestParts) throws HostInvokerException {
            return null;
        }
        
    }

}

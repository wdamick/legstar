package com.legstar.host.invoke;

import java.util.Map;

import org.apache.commons.configuration.HierarchicalConfiguration;

import com.legstar.config.Config;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.host.HostData;
import com.legstar.host.access.DirectHostAccessStrategy;
import com.legstar.host.access.HostAccessStrategy;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.LegStarHeaderPart;
import com.legstar.messaging.LegStarMessage;

/**
 * Test the generic Invoker.
 */
public class AbstractInvokerTest extends AbstractTestInvokers {
    
    
    public void testInvoke() throws Exception {
        LegStarAddress address = new LegStarAddress("TheMainframe");
        HierarchicalConfiguration generalConfig = Config.loadGeneralConfig(CONFIG_FILE);
        HierarchicalConfiguration endpointConfig = Config.loadAddressConfiguration(
                generalConfig, address);
        HostAccessStrategy hostAccessStrategy = new DirectHostAccessStrategy(endpointConfig);;
        CicsProgram hostProgram = new CicsProgram("lsfileae.properties");

        AbstractInvokerImpl invoker = new AbstractInvokerImpl(hostAccessStrategy, address, hostProgram);
        byte[] responseBytes = invoker.invoke("lsfileae", HostData.toByteArray(LSFILEAE_BYTES_REQUEST));
        assertTrue(responseBytes !=  null);
        assertEquals(LSFILEAE_BYTES_REPLY, HostData.toHexString(responseBytes));
        
    }
    
    /**
     * A mock implementation of the AbstractInvoker.
     *
     */
    public final class AbstractInvokerImpl extends AbstractInvoker {

        public AbstractInvokerImpl(HostAccessStrategy hostAccessStrategy,
                LegStarAddress completeAddress, CicsProgram hostProgram)
                throws HostInvokerException {
            super(hostAccessStrategy, completeAddress, hostProgram);
        }

        public void invoke(String requestID, ICobolComplexBinding ccbin,
                ICobolComplexBinding ccbout) throws HostInvokerException {
        }

        public void invoke(String requestID,
                Map<String, ICobolComplexBinding> inParts,
                Map<String, ICobolComplexBinding> outParts)
                throws HostInvokerException {
        }

        public byte[] invoke(String requestID, byte[] requestBytes)
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

        public Map<String, byte[]> invoke(String requestID,
                Map<String, byte[]> requestParts) throws HostInvokerException {
            return null;
        }
        
    }

}

package com.legstar.messaging.impl;

import java.util.ArrayList;
import java.util.Map;

import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.host.HostException;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.ContainerPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.HostMessageFormatException;
import com.legstar.messaging.LegStarHeaderPart;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarMessagePart;

/**
 * This is an implementation of the <code>LegStarMessage</code> that assumes
 * all message parts are built from COBOL binding objects.
 */
public class LegStarMessageImpl extends LegStarMessage {

	/** Serial version ID.  */
	private static final long serialVersionUID = -2901290130092178692L;


	/**
	 * No-arg constructor.
	 * @throws HeaderPartException if message cannot be built.
	 */
	public LegStarMessageImpl() throws HeaderPartException {
		super();
	}
	
	/**
	 * Constructor for situations where the header set of parameters is
	 * known in advance.
	 * @param keyValues the set of key/value parameters to create a header from
	 * @throws HeaderPartException if building header fails
	 */
	public LegStarMessageImpl(
			final Map < String, Object > keyValues) throws HeaderPartException {
		LegStarHeaderPart headerPart = new LegStarHeaderPart(keyValues, 0);
		setHeaderPart(headerPart);
		setDataParts(new ArrayList < LegStarMessagePart >());
	}
	
	/**
	 * Constructor for situations where a LegStar message exists and needs
	 * to be adapted.
	 * @param legStarMessage the original message
	 * @throws HeaderPartException if fails to adapt
	 */
	public LegStarMessageImpl(
			final LegStarMessage legStarMessage) throws HeaderPartException {
		setHeaderPart(legStarMessage.getHeaderPart());
		setDataParts(legStarMessage.getDataParts());
	}
	
    /**
     * Formats a message part by converting a java data object to a host
     * byte array and then wrapping that array in LegStar message part.
     * @param binding the COBOL binding decorating the java data object
     * @param hostBytesSize the projected host byte array size if known
     * in advance, zero otherwise.
     * @param hostCharset the host character set
     * @param containerName the container name if this part should be a
     * named container, null otherwise
     * @throws HostMessageFormatException if formatting fails
     */
    public void addMessagePart(
            final ICobolComplexBinding binding,
            final int hostBytesSize,
            final String hostCharset,
            final String containerName) throws HostMessageFormatException {
        
        try {
            int size = hostBytesSize;
            
            /* If the host byte size was not passed by the caller, it
             * is calculated from the COBOL binding. */
            if (size == 0) {
                size = binding.calcByteLength();
            }
            
            /* Convert Java data object to a host byte array */
            byte[] hostBytes = new byte[size];
            int dataLength = CobolTransformer.marshal(
                    CobolTransformer.getCobolConverters(hostCharset),
                    binding, hostBytes);
            
            /* Wrap the host byte array in message part */
            LegStarMessagePart part;
            if (containerName == null || containerName.length() == 0) {
                part = new CommareaPart(hostBytes);
            } else {
                part = new ContainerPart(containerName, hostBytes);
            }
            /* Adjust the payload size if needed */
            if (dataLength < size) {
                part.setPayloadSize(dataLength);
            }
            addDataPart(part);
            
        } catch (HostMarshalException e) {
            throw new HostMessageFormatException(e);
        } catch (HostException e) {
            throw new HostMessageFormatException(e);
        }
        
    }
    
    /**
     * Populates a binding inner java data object from the content of
     * a LegStar message part.
     * @param binding the binding object to be populated
     * @param hostCharset the host character set
     * @param containerName the container name if this part is a named
     *  container, null otherwise
     * @throws HostMessageFormatException if data part cannot be processed
     */
    public void getBindingFromPart(
    		final ICobolComplexBinding binding,
    		final String hostCharset,
            final String containerName) throws HostMessageFormatException {
        
    	try {
			/* If there are no data parts, we return with no java data object */
			if (getDataParts().size() == 0) {
				return;
			}
			LegStarMessagePart part;
			if (containerName == null || containerName.length() == 0) {
				/* When no container is specified, only the first data
				 * part is processed. */
                part = getDataParts().get(0);
			} else {
				part = lookupDataPart(containerName);
			}
			if (part == null) {
				return; 
			}
            CobolTransformer.unmarshal(
                    CobolTransformer.getCobolConverters(hostCharset),
                    part.getContent(),
                    binding);
		} catch (HostUnmarshalException e) {
            throw new HostMessageFormatException(e);
		}
   	
    }

    /**
     * Populates a list of bindings inner java data objects from corresponding
     * parts. Each binding must be associated with a part ID (usually a
     * container name).
     * @param bindingsMap a map of part IDs to bindings
     * @param hostCharset the host character set
     * @throws HostMessageFormatException if data parts cannot be processed
     */
    public void getBindingsFromParts(
    		final Map < String, ICobolComplexBinding > bindingsMap,
    		final String hostCharset) throws HostMessageFormatException {
    	try {
			for (LegStarMessagePart part : getDataParts()) {
				ICobolComplexBinding binding = bindingsMap.get(part.getID());
				if (binding == null) {
					continue;
				}
				if (part.getContent() == null) {
					continue;
				}
			    CobolTransformer.unmarshal(
			            CobolTransformer.getCobolConverters(hostCharset),
			            part.getContent(),
			            binding);
			}
		} catch (HostUnmarshalException e) {
			throw new HostMessageFormatException(e);
		}
    }
    
	

}

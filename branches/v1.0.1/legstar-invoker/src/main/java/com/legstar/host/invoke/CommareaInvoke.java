/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.host.invoke;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.CobolMarshalVisitor;
import com.legstar.coxb.CobolUnmarshalVisitor;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.convert.CobolConverters;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.host.HostException;
import com.legstar.host.access.HostAccessStrategy;
import com.legstar.host.access.HostAccessStrategyException;
import com.legstar.host.access.HostAccessStrategyFactory;
import com.legstar.messaging.Address;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.HeaderPart;
import com.legstar.messaging.Message;
import com.legstar.messaging.MessagePart;
import com.legstar.messaging.Request;
import com.legstar.config.Config;


/**
 * An invoker has the capability to run a host program using java object trees
 * as input and output structures. The host program attributes are loaded from
 * a properties files and an XML configuration is expected to provide host
 * access parameters. The actual transport used is abstracted by dynamically
 * loading a host accessor based on a factory name from the XML configuration.
 */
public class CommareaInvoke implements HostInvoke {
	
	/** Logger. */
	private static final Log LOG = LogFactory.getLog(CommareaInvoke.class);
	
	/** XML Configuration as a class field, in order to load only once. */
	private static HierarchicalConfiguration sGeneralConfig;
	
	/** Host program attributes. */
	private ProgramAttributes mProgramAttr;
	
	/** Host endpoint targeted. */
	private Address mAddress;
	
	/** Cobol to Java converters. */
	private CobolConverters mCobolConverters;
	
	/** An abstract host access capability. */
	private HostAccessStrategy mHostAccessStrategy;
	
	/**
	 * An Invoker is constructed from configuration files and for a particular
	 * host address.
	 * @param generalConfigFileName an XML configuration file name
	 * @param address the host address
	 * @param programAttributesFileName the host program attributes properties
	 * file
	 * @throws HostInvokeException in construction fails
	 */
	public CommareaInvoke(
			final String generalConfigFileName,
			final Address address,
			final String programAttributesFileName) throws HostInvokeException {
		
		/* Load program attributes from a properties file */
		mProgramAttr = new ProgramAttributes(programAttributesFileName);
		
		/* Load the XML configuration, if necessary */
		if (sGeneralConfig == null) {
			sGeneralConfig = loadGeneralConfig(generalConfigFileName);
		}
		
		/* Load desired endpoint configuration */
		HierarchicalConfiguration endpointConfig = null;
		try {
			endpointConfig = Config.loadAddressConfiguration(
					sGeneralConfig, address);
		} catch (ConfigurationException e) {
			throw new HostInvokeException(e);
		}
		
		mAddress = new Address(address, endpointConfig);

		/* Load a host access strategy */
		try {
			mHostAccessStrategy =
				HostAccessStrategyFactory.createAccessStrategy(endpointConfig);
		} catch (HostAccessStrategyException e) {
			throw new HostInvokeException(e);
		}
	
		/* Setup cobol converters with the target character set */
		CobolContext cobolContext = new CobolContext();
		cobolContext.setHostCharsetName(mAddress.getHostCharset());
		
		/* Select a conversion strategy */ 
		mCobolConverters = new CobolSimpleConverters(cobolContext);
	}
	
	/**
	 * Invoke a commarea-driven program.
	 * @param requestID an identifier for this request (used for tracing)
	 * @param ccbin the input object tree
	 * @param ccbout the output object tree
	 * @throws HostInvokeException if invoke fails
	 */
	public final void invoke(
			final String requestID,
			final ICobolComplexBinding ccbin,
			final ICobolComplexBinding ccbout) throws HostInvokeException {
		
		long start = System.currentTimeMillis();
		if (LOG.isDebugEnabled()) {
 			LOG.debug("Invoke Commarea started");
		}
		/* Allocate a host data buffer for the request */
		byte[] hostInputBytes = new byte[mProgramAttr.getLength()];
		
		/* Convert from java to host */
		int dataLength = marshal(ccbin, hostInputBytes);
		
		/* Adjust the program data length if needed */
		if (dataLength < mProgramAttr.getDataLength()) {
			mProgramAttr.setDataLength(dataLength);
		}
		
		/* Create a request instance and call the host program */
		Request request = createCommareaRequest(requestID, mAddress,
				hostInputBytes);
		try {
			mHostAccessStrategy.invoke(request);
		} catch (HostAccessStrategyException e) {
			throw new HostInvokeException(e);
		}
		
		/* The request might have failed */
		if (request.getException() != null) {
			throw new HostInvokeException(request.getException());
		}
		/* Since this is a commarea driven program, a single data
		 * part is expected back  */
		if (request.getResponseMessage().getDataParts().size() > 0) {
			unmarshal(request.getResponseMessage().getDataParts().
					get(0).getContent(), ccbout);
		}

		if (LOG.isDebugEnabled()) {
			long end = System.currentTimeMillis();
			LOG.debug("Invoke Commarea ended. elapse: "
					+ Long.toString(end - start) + " ms");
		}
	}
	
    /**
     * Loads an XML configuration from file.
     * @param configFileName the configuration file name
     * @return the in-memory XML configuration
     * @throws HostInvokeException if configuration failed to load
     */
    private synchronized HierarchicalConfiguration loadGeneralConfig(
            final String configFileName) throws  HostInvokeException {
		/* Because the original test for nullity is not guarded, we might
		 * still get into this synchronized code while some other thread
		 * already assigned the static variable. */
    	if (sGeneralConfig != null) {
			return sGeneralConfig;
		}
        try {
        	HierarchicalConfiguration generalConfig =
        		Config.loadGeneralConfig(configFileName);
            return generalConfig;
       } catch (ConfigurationException e) {
            throw new HostInvokeException(e);
        }
     }
    
	/**
	 * Creates a request for a commarea-driven program.
	 * @param requestID a request traceability identifier
	 * @param address the host endpoint
	 * @param hostInputBytes the input binary content
	 * @return the request ready to be submitted
	 * @throws HostInvokeException if failed to create request
	 */
	private Request createCommareaRequest(
			final String requestID,
			final Address address,
			final byte[] hostInputBytes) throws HostInvokeException {
		
		List < MessagePart > dataParts = new ArrayList < MessagePart >();
		dataParts.add(new CommareaPart(hostInputBytes));
		HeaderPart headerPart;
		try {
			headerPart = new HeaderPart(
				mProgramAttr.getProgramAttrMap(), dataParts.size(),
				mAddress.getHostCharset());
		} catch (UnsupportedEncodingException e) {
			throw new HostInvokeException(e);
		}
		Message requestMessage = new Message(headerPart, dataParts);
		return new Request(requestID, address, requestMessage);
	}

	/**
	 * Convert JAXB object tree to a host buffer.
	 * 
	 * @param ccbin input object tree
	 * @param hostInputBytes a byte buffer which will be filled with host data
	 * @return the actual number of bytes marshaled
	 * @throws MarshalException if marshaling failed
	 */
	private int marshal(
			final ICobolComplexBinding ccbin,
			final byte[] hostInputBytes) throws MarshalException {

		int bytesMarshalled = 0;
		long start = System.currentTimeMillis();
		if (LOG.isDebugEnabled()) {
 			LOG.debug("Marshaling started");
		}

		/* create the outbound buffer by marshalling the java object tree */
		CobolMarshalVisitor mv =
			new CobolMarshalVisitor(hostInputBytes, 0, mCobolConverters);
		try {
			/* Traverse the object structure, visiting each node with the
			 *  visitor */
			ccbin.accept(mv);
		} catch (HostException he) {
			throw new MarshalException(he);
		}
		/* Get the actual bytes marshalled */
		bytesMarshalled = mv.getOffset();
 
		if (LOG.isDebugEnabled()) {
			long end = System.currentTimeMillis();
			LOG.debug("Marshaling ended. Processed: "
					+ Integer.toString(bytesMarshalled) + " bytes "
					+ "elapse:"
					+ Long.toString(end - start) + " ms");
		}
		
		return bytesMarshalled;
	}

	/**
	 * Convert a host buffer to a JAXB object tree.
	 * 
	 * @param hostOutputBytes the host data buffer
	 * @param ccbout output object tree
	 * @throws UnmarshalException if unmarshaling failed
	 */
	private void unmarshal(
			final byte[] hostOutputBytes,
			final ICobolComplexBinding ccbout) throws UnmarshalException {

		int bytesUnmarshalled = 0;
    	long start = System.currentTimeMillis();
		if (LOG.isDebugEnabled()) {
			LOG.debug("Unmarshaling started");
		}

		/* Unmarshal the raw host reply into a java object tree */
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(
				hostOutputBytes, 0, mCobolConverters);
		try {
			/* Traverse the object structure, visiting each node with the
			 *  visitor */
			ccbout.accept(uv);
		} catch (HostException he) {
			throw new UnmarshalException(he);
		}
		/* Get the actual bytes unmarshalled */
		bytesUnmarshalled = uv.getOffset();

		if (LOG.isDebugEnabled()) {
			long end = System.currentTimeMillis();
			LOG.debug("Unmarshalling ended Processed: "
					+ Integer.toString(bytesUnmarshalled) + " bytes "
					+ "elapse:"
					+ Long.toString(end - start) + " ms");
		}
		
	}

	/**
	 * @return the hpst address
	 */
	public final Address getAddress() {
		return mAddress;
	}

	/**
	 * @return the cobol converters
	 */
	public final CobolConverters getCobolConverters() {
		return mCobolConverters;
	}

	/**
	 * @return the host access strategy
	 */
	public final HostAccessStrategy getHostAccessStrategy() {
		return mHostAccessStrategy;
	}

	/**
	 * @return the programme attributes
	 */
	public final ProgramAttributes getProgramAttr() {
		return mProgramAttr;
	}

}

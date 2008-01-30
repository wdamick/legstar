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
package com.legstar.c2ws;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.c2ws.util.C2wsLog;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.LegStarHeaderPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.HostReceiveException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarMessagePart;

/**
 * This class provides the capability to invoke a target web service on
 * behalf of a host requestor. The host requestor identifies the target
 * service using a logical name which is matched with an active configuration
 * in order to locate the parameters that are necessary for a successful
 * SOAP method execution. This class delegates the actual call to an
 * adapter class.
 */
public class C2wsInvoker {

	/** Identifies the requested service name. 
	 * (TODO Should go to messaging.Constants) */
	private static final String SERVICE_NAME_KEY = "ServiceName";

	/** Configuration key to a name of a class implementing an adapter. */ 
	private static final String ADAPTER_NAME_KEY = "c2ws.adapter";
	
	/** Default name of a class implementing an adapter. */ 
	private static final String DEFAULT_ADAPTER_NAME =
		"com.legstar.c2ws.reflect.C2wsReflectAdapter";
	
	/** Configuration key for the host character set. */ 
	private static final String HOST_CHARSET_KEY = "c2ws.host.charset";
	
	/** Configuration holds all the target Web Services descriptions. */
	private C2wsConfigurationManager mC2wsConfigManager;
	
	/** Logger. */
	private static final Log LOG =	LogFactory.getLog(C2wsInvoker.class);
	
	/** Enhanced logger with correlation id. */
	private C2wsLog mLog = new C2wsLog(LOG);
	
	/** A concrete implementation of a c2ws adapter. */
	private C2wsAdapter mAdapter;
	
	/**
	 * Constructor for a given configuration.
	 * @param cxid a Correlation id to use with traces
	 * @param configManager a configuration holding target Web services
	 *  descriptors.
	 * @throws C2wsInvokerException if invoker fails to instanciate
	 */
	public C2wsInvoker(
			final String cxid,
			final C2wsConfigurationManager configManager)
			throws C2wsInvokerException {
		mLog.setCorrelationId(cxid);
		mC2wsConfigManager = configManager;
		mAdapter = loadAdapter();
		mAdapter.setCorrelationId(cxid);
		/* Get a host character set from the configuration */
		String hostCharset =
			mC2wsConfigManager.getC2wsConfig().getString(HOST_CHARSET_KEY);
		if (hostCharset != null && hostCharset.length() > 0) {
			mAdapter.setHostCharset(hostCharset);
		}
	}
	
	/**
	 * Invoke a target Web Service, locating its descriptor and then emittting
	 * a SOAP request. This is for a one input/one output exchange pattern.
	 * @param requestMessage describes the request
	 * @return a response message
	 * @throws C2wsInvokerException if invoke fails
	 */
	public final LegStarMessage invoke(
			final LegStarMessage requestMessage) throws C2wsInvokerException {
		try {
			if (mLog.isDebugEnabled()) {
				mLog.debug("Entered invoke with message " + requestMessage);
			}
			String serviceName = getServiceName(requestMessage);
			C2wsWSDescriptor wsd =
				mC2wsConfigManager.getWebServiceDescriptor(serviceName);
			byte[] responseBytes = mAdapter.invoke(wsd,
					requestMessage.getDataParts().get(0).getContent());
			List < LegStarMessagePart > dataParts =
				new ArrayList < LegStarMessagePart >();
			dataParts.add(new CommareaPart(responseBytes));
			LegStarHeaderPart headerPart = new LegStarHeaderPart();
			headerPart.setDataPartsNumber(dataParts.size());
			LegStarMessage responseMessage = new LegStarMessage();
			responseMessage.setHeaderPart(headerPart);
			responseMessage.setDataParts(dataParts);
			if (mLog.isDebugEnabled()) {
				mLog.debug("invoke returned with message " + responseMessage);
			}
			return responseMessage;
		} catch (HeaderPartException e) {
			throw new C2wsInvokerException(e);
		} catch (HostReceiveException e) {
			throw new C2wsInvokerException(e);
		} catch (C2wsConfigurationException e) {
			throw new C2wsInvokerException(e);
		} catch (C2wsAdapterException e) {
			throw new C2wsInvokerException(e);
		}
	}
	
	/**
	 * Locates and instanciates an adapter implementation.
	 * @return a valid adapter
	 * @throws C2wsInvokerException if adapter cannot be instanciated
	 */
	private C2wsAdapter loadAdapter() throws C2wsInvokerException {
		
		/* First attempt to get the adapter name from the configuration */
		String adapterClassName =
			mC2wsConfigManager.getC2wsConfig().getString(ADAPTER_NAME_KEY);
		
		/* If no entry found in configuration, use the default */
		if (adapterClassName == null || adapterClassName.length() == 0) {
			adapterClassName = DEFAULT_ADAPTER_NAME;
		}
		try {
			Class < ? > adapterClass = Class.forName(adapterClassName);
			return (C2wsAdapter) adapterClass.newInstance();
		} catch (ClassNotFoundException e) {
			throw new C2wsInvokerException(e);
		} catch (InstantiationException e) {
			throw new C2wsInvokerException(e);
		} catch (IllegalAccessException e) {
			throw new C2wsInvokerException(e);
		}
	}
	
	/**
	 * Extracts the requested service name from the meta-data sent by the host.
	 * @param requestMessage the message received from host
	 * @return the requested service name
	 * @throws HostReceiveException if meta-data is wrong
	 */
	public static String getServiceName(
			final LegStarMessage requestMessage) throws HostReceiveException {
		String jsonString;
		try {
			jsonString = requestMessage.getHeaderPart().getJsonString();
		} catch (HeaderPartException e) {
			throw new HostReceiveException(e);
		}
		if (jsonString == null || jsonString.length() == 0) {
			throw new HostReceiveException("JSON string is empty");
		}
		/* We expect a very simple JSON expression so there is no need
		 * for a full blown JSON tokenizer here. */
		String[] tokens = jsonString.split("(\"\\s*:\\s*\")");
		if (tokens.length != 2) {
			throw new HostReceiveException("JSON string [" + jsonString
					+ "] is not recognized");
		}
		String key = tokens[0].substring(2, tokens[0].length()).trim();
		String val = tokens[1].substring(0, tokens[1].length() - 2).trim();
		if (key.compareTo(SERVICE_NAME_KEY) != 0) {
			throw new HostReceiveException("JSON string [" + jsonString
					+ "] contains unknown key");
		}
		return val;
	}

}

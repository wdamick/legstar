/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.c2ws.servlet;

import java.util.ArrayList;
import java.util.List;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.c2ws.C2wsAdapter;
import com.legstar.c2ws.C2wsAdapterException;
import com.legstar.c2ws.C2wsConfigurationException;
import com.legstar.c2ws.C2wsInvokeException;
import com.legstar.c2ws.C2wsWSDescriptor;
import com.legstar.c2ws.util.C2wsLog;
import com.legstar.util.JAXBAnnotationException;
import com.legstar.util.JAXBElementDescriptor;
import com.legstar.util.JaxbUtil;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.HostReceiveException;
import com.legstar.messaging.LegStarHeaderPart;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.LegStarMessagePart;

/**
 * This proxy servlet is generic but expects a web.xml that gives the target
 * web service description. This way implementing an actual proxy is a matter
 * of setting up the right web.xml.
 * LegStar mainframe clients are expected to call the corresponding servlet
 * URL and get access to the target web service.
 *
 */
public class C2wsProxy extends javax.servlet.http.HttpServlet
implements javax.servlet.Servlet {

	/** The serial ID. */
	static final long serialVersionUID = 1L;

	/** The only content-type supported by this class. */
	private static final String BINARY_CONTENT_TYPE = "binary/octet-stream";

	/** The special http header for correlation ids. */
	private static final String CORRELATION_ID_HDR = "Correlation-id";

	/** Configuration key to a name of a class implementing an adapter. */ 
	private static final String ADAPTER_CLASSNAME_KEY = "c2ws.adapter";

	/** Default name of a class implementing an adapter. */ 
	private static final String DEFAULT_ADAPTER_CLASSNAME =
		"com.legstar.c2ws.reflect.C2wsReflectAdapter";

	/** Configuration key for the host character set. */ 
	private static final String HOST_CHARSET_KEY = "c2ws.hostCharset";

	/** Default value for host character set. */ 
	private static final String DEFAULT_HOST_CHARSET = "IBM01140";

	/** URL locating target Web service WSDL. */ 
	private static final String WSDL_URL_KEY = "c2ws.wsdlUrl";

	/** Target Web service WSDL namespace. */ 
	private static final String WSDL_TARGET_NAMESPACE_KEY =
		"c2ws.wsdlTargetNamespace";

	/** Target Web service WSDL port name. */ 
	private static final String WSDL_PORT_NAME_KEY =
		"c2ws.wsdlPortName";

	/** Target Web service WSDL service name. */ 
	private static final String WSDL_SERVICE_NAME_KEY =
		"c2ws.wsdlServiceName";

	/** Request JAXB type. */ 
	private static final String REQUEST_JAXB_TYPE_KEY =
		"c2ws.requestJaxbType";

	/** Request JAXB package name. */ 
	private static final String REQUEST_JAXB_PACKAGE_NAME_KEY =
		"c2ws.requestJaxbPackageName";

	/** Response JAXB type. */ 
	private static final String RESPONSE_JAXB_TYPE_KEY =
		"c2ws.responseJaxbType";

	/** Response JAXB package name. */ 
	private static final String RESPONSE_JAXB_PACKAGE_NAME_KEY =
		"c2ws.responseJaxbPackageName";

	/** Actual provider of Web Service access capabilities. */
	private C2wsAdapter mAdapter;

	/** Describes the target Web Service. */
	private C2wsWSDescriptor mWSDescriptor;

	/** This servlet proxy name. */
	private static final String PROXY_NAME = "JvmqueryProxy";

	/** Logger. */
	private static final Log LOG =  LogFactory.getLog(C2wsProxy.class);

	/** Enhanced logger with correlation id. */
	private C2wsLog mLog = new C2wsLog(LOG);

	/** {@inheritDoc} */
	protected void doPost(
			final HttpServletRequest request,
			final HttpServletResponse response) throws ServletException {

		/* Use correlation id received in http header. This allows logs on
		 * this side to be easily correlated with the mainframe logs. */
		String cxid = request.getHeader(CORRELATION_ID_HDR);
		mLog.setCorrelationId(cxid);
		if (mLog.isDebugEnabled()) {
			mLog.debug(PROXY_NAME + " started for " + request.getRemoteHost());
		}

		/* Make sure this is a Mainframe LegStar request. */
		if (request.getContentType().compareToIgnoreCase(
				BINARY_CONTENT_TYPE) != 0) {
			throw new ServletException("Content type "
					+ request.getContentType()
					+ " is not supported by " + PROXY_NAME);
		}

		try {
			LegStarMessage requestMessage = new LegStarMessage();
			requestMessage.recvFromHost(request.getInputStream());
			LegStarMessage responseMessage = invoke(cxid, requestMessage);
			response.setContentType(BINARY_CONTENT_TYPE);
			pipe(responseMessage.sendToHost(), response.getOutputStream());
		} catch (HeaderPartException e) {
			throw (new ServletException(e));
		} catch (HostReceiveException e) {
			throw (new ServletException(e));
		} catch (C2wsInvokeException e) {
			throw (new ServletException(e));
		} catch (IOException e) {
			throw (new ServletException(e));
		}

		if (mLog.isDebugEnabled()) {
			mLog.debug(PROXY_NAME + " ended");
		}
	} 

	/**
	 * Invoke a target Web Service, using its descriptor and then emitting
	 * a SOAP request. This is for a one input/one output exchange pattern.
	 * @param cxid a correlation ID sent by the client
	 * @param requestMessage describes the request
	 * @return a response message
	 * @throws C2wsInvokeException if invoke fails
	 */
	public final LegStarMessage invoke(
			final String cxid,
			final LegStarMessage requestMessage) throws C2wsInvokeException {
		try {
			if (mLog.isDebugEnabled()) {
				mLog.debug("Entered invoke with message " + requestMessage);
			}
			mAdapter.setCorrelationId(cxid);
			byte[] responseBytes = mAdapter.invoke(mWSDescriptor,
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
			throw new C2wsInvokeException(e);
		} catch (C2wsAdapterException e) {
			throw new C2wsInvokeException(e);
		}
	}

	/** {@inheritDoc} */
	public final void init(
			final ServletConfig config) throws ServletException {
		super.init(config);

		if (mLog.isDebugEnabled()) {
			mLog.debug("Initializing " + PROXY_NAME);
		}

		try {
			/* Initialize the target web service description. */
			mWSDescriptor = getInitWSDescriptor();
			if (mLog.isDebugEnabled()) {
				mLog.debug("Web Service descriptor:");
				mLog.debug(mWSDescriptor.toString());
			}

			/* Setup a Mainframe character set to use when marshaling/
			 * unmarshaling.  */
			String hostCharset = getInitParameter(
					HOST_CHARSET_KEY, DEFAULT_HOST_CHARSET);

			/* Load an adapter which implements the actual Web Service client */
			String adapterClassName = getInitParameter(
					ADAPTER_CLASSNAME_KEY, DEFAULT_ADAPTER_CLASSNAME);
			mAdapter = loadAdapter(adapterClassName);
			mAdapter.setHostCharset(hostCharset);
		} catch (C2wsConfigurationException e) {
			throw new ServletException(e);
		}
	}

	/**
	 * Try to locate an initialization parameter providing default value
	 * if not found.
	 * @param parameterName the parameter name
	 * @param defaultValue the default value if not found
	 * @return the parameter value
	 */
	private String getInitParameter(
			final String parameterName, final String defaultValue) {
		String value = getServletConfig().getInitParameter(parameterName);
		if (value == null || value.length() == 0) {
			value = defaultValue;
			if (mLog.isDebugEnabled()) {
				mLog.debug("Parameter " + parameterName + " not found."
						+ " Using default value: " + value);
			}
		} else {
			if (mLog.isDebugEnabled()) {
				mLog.debug("Parameter " + parameterName + " found."
						+ " Using value: " + value);
			}
		}
		return value;
	}

	/**
	 * @return a description of the Web Service this servlet provides a proxy
	 * for. All parameters are expected to be passed as servlet initialization
	 * parameters.
	 * @throws C2wsConfigurationException if configuration is invalid
	 */
	private C2wsWSDescriptor getInitWSDescriptor()
	throws C2wsConfigurationException {
		try {
			C2wsWSDescriptor wsd = new C2wsWSDescriptor();
			wsd.setWsdlUrl(
					getInitParameter(WSDL_URL_KEY, null));
			wsd.setWsdlTargetNamespace(
					getInitParameter(WSDL_TARGET_NAMESPACE_KEY, null));
			wsd.setWsdlPort(
					getInitParameter(WSDL_PORT_NAME_KEY, null));
			wsd.setWsdlName(
					getInitParameter(WSDL_SERVICE_NAME_KEY, null));
			JAXBElementDescriptor jaxbRequest = new JAXBElementDescriptor(
					getInitParameter(REQUEST_JAXB_PACKAGE_NAME_KEY, null),
					getInitParameter(REQUEST_JAXB_TYPE_KEY, null));
			wsd.setRequestElementDescriptor(jaxbRequest);
			JAXBElementDescriptor jaxbResponse = new JAXBElementDescriptor(
					getInitParameter(RESPONSE_JAXB_PACKAGE_NAME_KEY, null),
					getInitParameter(RESPONSE_JAXB_TYPE_KEY, null));
			wsd.setResponseElementDescriptor(jaxbResponse);
			return wsd;
		} catch (JAXBAnnotationException e) {
			throw new C2wsConfigurationException(e);
		}
	}

	/**
	 * Locates and instantiates an adapter implementation.
	 * @param adapterClassName the adapter class name
	 * @return a valid adapter
	 * @throws C2wsConfigurationException if adapter cannot be instantiated
	 */
	private C2wsAdapter loadAdapter(
			final String adapterClassName) throws C2wsConfigurationException {
		try {
			Class < ? > adapterClass = JaxbUtil.loadClass(adapterClassName);
			return (C2wsAdapter) adapterClass.newInstance();
		} catch (ClassNotFoundException e) {
			throw new C2wsConfigurationException(e);
		} catch (InstantiationException e) {
			throw new C2wsConfigurationException(e);
		} catch (IllegalAccessException e) {
			throw new C2wsConfigurationException(e);
		}
	}

	/**
	 * Simple piping using an intermediary buffer.
	 * @param in the input stream
	 * @param out the output stream
	 * @throws IOException if piping fails
	 */
	private void pipe(
			final InputStream in,
			final OutputStream out) throws IOException {
		byte[] buffer = new byte[1024];
		int r;
		while ((r = in.read(buffer)) > 0) {
			out.write(buffer, 0, r);
		}
	}

	/**
	 * @return the web service client adapter
	 */
	public final C2wsAdapter getAdapter() {
		return mAdapter;
	}

	/**
	 * @param adapter the web service client adapter to set
	 */
	public final void setAdapter(final C2wsAdapter adapter) {
		mAdapter = adapter;
	}

	/**
	 * @return the web service descriptor
	 */
	public final C2wsWSDescriptor getWSDescriptor() {
		return mWSDescriptor;
	}

	/**
	 * @param descriptor the web service descriptor to set
	 */
	public final void setWSDescriptor(final C2wsWSDescriptor descriptor) {
		mWSDescriptor = descriptor;
	}
}

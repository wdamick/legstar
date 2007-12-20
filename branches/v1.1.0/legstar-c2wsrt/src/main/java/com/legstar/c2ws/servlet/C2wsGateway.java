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
package com.legstar.c2ws.servlet;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.c2ws.C2wsConfigurationException;
import com.legstar.c2ws.C2wsConfigurationManager;
import com.legstar.c2ws.C2wsInvokerException;
import com.legstar.c2ws.C2wsInvoker;
import com.legstar.c2ws.util.C2wsLog;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.HostReceiveException;
import com.legstar.messaging.Message;

/**
 * Servlet implementation class for Servlet: C2wsGateway. This servlet is 
 * activated whenever a host emits a request to invoke a Web Service.
 * This class acts as a gateway between the host and the target Web Service.
 *
 */
 public class C2wsGateway extends javax.servlet.http.HttpServlet
 		implements javax.servlet.Servlet {
	
	/** serialVersionUID. */
	private static final long serialVersionUID = -292327797640768625L;
	
	/** Config file name init param key. */
	public static final String CONFIG_PARAM = "c2wsrt.config";
	
	/** The only content-type supported by this class. */
	private static final String BINARY_CONTENT_TYPE = "binary/octet-stream";
	
	/** The special http header for correlation ids. */
	private static final String CORRELATION_ID_HDR = "Correlation-id";
	
	/** Configuration holds all the target Web Services descriptions. */
	private C2wsConfigurationManager mC2wsConfigManager;
	
	/** Logger. */
	private static final Log LOG =	LogFactory.getLog(C2wsGateway.class);
	
	/** Enhanced logger with correlation id. */
	private C2wsLog mLog = new C2wsLog(LOG);
	
	 /** (non-Java-doc).
	 * @see javax.servlet.http.HttpServlet#HttpServlet()
	 */
	public C2wsGateway() {
		super();
	}
	
	/** (non-Javadoc).
	 * @see javax.servlet.GenericServlet#init(javax.servlet.ServletConfig)
	 * {@inheritDoc}
	 */
	public final void init(
			final ServletConfig config) throws ServletException {
		super.init(config);
		
		if (mLog.isDebugEnabled()) {
			mLog.info("Initializing servlet with " + CONFIG_PARAM
					+ " configuration parameter.");
		}
		
		String c2wsConfigFileName = config.getInitParameter(CONFIG_PARAM);
		if (c2wsConfigFileName == null || c2wsConfigFileName.length() == 0) {
			throw new ServletException(
					"Web.xml does not contain the " + CONFIG_PARAM
					+ " configuration parameter.");
		}
		
		try {
			mC2wsConfigManager =
				new C2wsConfigurationManager(c2wsConfigFileName);
		} catch (C2wsConfigurationException e) {
			throw new ServletException(e);
		}
	}
	
	/** (non-Java-doc).
	 * @see javax.servlet.http.HttpServlet#doPost(
	 * HttpServletRequest request, HttpServletResponse response)
	 * {@inheritDoc}
	 */
	protected final void doPost(
			final HttpServletRequest request,
			final HttpServletResponse response)
				throws ServletException, IOException {
		
		/* Use correlation id received in http header */
		String cxid = request.getHeader(CORRELATION_ID_HDR);
		mLog.setCorrelationId(cxid);
		if (mLog.isDebugEnabled()) {
			mLog.debug("C2wsGateway started for " + request.getRemoteHost());
		}
		
		if (request.getContentType().compareToIgnoreCase(
				BINARY_CONTENT_TYPE) != 0) {
			throw new ServletException("Content type "
					+ request.getContentType()
					+ " is not supported by C2wsGateway");
		}

		try {
			Message requestMessage = new Message();
			requestMessage.recvFromHost(request.getInputStream());
			C2wsInvoker invoker =
				new C2wsInvoker(cxid, mC2wsConfigManager);
			Message responseMessage = invoker.invoke(requestMessage);
			response.setContentType(BINARY_CONTENT_TYPE);
			pipe(responseMessage.sendToHost(), response.getOutputStream());
		} catch (HeaderPartException e) {
			throw (new ServletException(e));
		} catch (HostReceiveException e) {
			throw (new ServletException(e));
		} catch (C2wsInvokerException e) {
			throw (new ServletException(e));
		}

		if (mLog.isDebugEnabled()) {
			mLog.debug("C2wsGateway ended");
		}
	}   	  	
	
	/** (non-Javadoc).
	 * @see javax.servlet.Servlet#getServletInfo()
	 * {@inheritDoc}
	 */
	public final String getServletInfo() {
		return super.getServletInfo() + "C2wsGateway";
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
	
}

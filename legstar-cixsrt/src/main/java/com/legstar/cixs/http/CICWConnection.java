/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/
package com.legstar.cixs.http;

import java.io.InputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.auth.AuthScope;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.commons.httpclient.HostConfiguration;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpState;
import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HttpStatus;
import org.apache.commons.httpclient.UsernamePasswordCredentials;
import org.apache.commons.httpclient.methods.ByteArrayRequestEntity;

import com.legstar.cixs.coxb.CIXSProgram;

/**
 * This class represents a connection to a host over HTTP
 * It uses POST of binary content to invoke a mainframe program.
 *
 * @author Fady Moussallam
 * 
 */
public class CICWConnection {

	/** Mime type of HTTP content. */
	private static final String BINARY_CONTENT_TYPE = "binary/octet-stream";

	/** An upper limit for any content length. */
	private static final long MAX_CONTENT_LEN = 4294967296L;

	/** Apache HTTP client instance. */
	private HttpClient mHttpClient;

	/** Apache HTTP host configuration. */
	private HostConfiguration mHttpConfig;

	/** Apache HTTP state. */
	private HttpState mHttpState;

	/** Host connectivity parameters. */
	private CICWHost mHost;

	/**
	 * Contructor for a given host connection.
	 * @param host Host connection parameters
	 * @param httpClient an Apache HTTP client
	 */
	public CICWConnection(final CICWHost host, final HttpClient httpClient) {

		mHost = host;
		mHttpConfig = new HostConfiguration();
		mHttpConfig.setHost(host.getHostIPAddress(), host.getHostIPPort());
		mHttpState = new HttpState();

		/* Save username and password as basic authentication credentials */
		UsernamePasswordCredentials upc =
			new UsernamePasswordCredentials(host.getHostUser(),
					host.getHostPassword());
		mHttpState.setCredentials(
				new AuthScope(host.getHostIPAddress(),
						host.getHostIPPort(), AuthScope.ANY_HOST), upc);
		mHttpClient = httpClient;
	}


	/**
	 * Invoke a host program.
	 * @param program the host program to execute
	 * @param request the host data to pass to the host program
	 * @return the raw response data from the host program
	 * @throws CICWException in invoke fails
	 */
	public final byte[] invoke(
			final CIXSProgram program,
			final byte[] request)
	throws CICWException {

		byte[] response = null;

		/* Make sure we have sizes for input and output */
		int requestLength = (request == null) ? 0 : request.length;
		int dataLength = (program.getDataLength() == 0) 
		? requestLength : program.getDataLength();
		int commLength = (program.getCommareaLength() == 0) 
		? requestLength : program.getCommareaLength();

		/* Make sure various sizes are valid */
		validateRequest(commLength, dataLength, requestLength);

		/* Create a post method to send the execution request */
		PostMethod postMethod = createPostMethod(program, dataLength,
				commLength, request);

		/* Set the send timeout*/
		mHttpClient.getParams().setSoTimeout(program.getExecuteTimeout());

		/* Execute the POST method and get the reply */
		int statusCode;
		try {
			statusCode = mHttpClient.executeMethod(
					mHttpConfig, postMethod, mHttpState);
		} catch (HttpException e) {
			e.printStackTrace();
			throw (new CICWException("HttpException " + e.getMessage()));
		} catch (IOException e) {
			e.printStackTrace();
			throw (new CICWException("IOException " + e.getMessage()));
		}
		response = getResponse(postMethod, statusCode);

		if (statusCode != HttpStatus.SC_OK) {
			processError(postMethod, response);
		}
		
		if (response == null) {
			if (program.getCommareaLength() == 0) {
				return null;
			} else {
				throw (new CICWException(
						"POST Method returned nothing while expecting="
						+ (new Integer(
								program.getCommareaLength())).toString()
								+ " bytes"));
			}
		}

		/* Check that the amount of data returned actually fits in the
		 * commarea length. Response is authorized to be smaller than
		 * the commarea length (variable size arrays) */
		if (postMethod.getResponseContentLength()
				> program.getCommareaLength()) {
			throw (new CICWException(
					"Commarea length is smaller than response"
					+ " received. Commarea="
					+ (new Integer(program.getCommareaLength())).toString()
					+ ", Received="
					+ (new Long(postMethod.
							getResponseContentLength())).toString()));
		}

		return response;
	}

	/**
	 * Verifies if length combinations makes sense.
	 * 
	 * @param commLength size of the commarea
	 * @param dataLength size of the input data within the commarea
	 * @param requestLength size of the buffer containing the requested data
	 * @throws CICWException if any of the validations fails
	 */
	private void validateRequest(
			final int commLength,
			final int dataLength,
			final int requestLength)
	throws CICWException {

		/* Check that the actual request data fits in the announced
		 *  DataLength */ 
		if (dataLength > requestLength) {
			throw (new CICWException(
					"Not enough data in request buffer. Expected="
					+ (new Integer(dataLength)).toString()
					+ ", requested="
					+ (new Integer(requestLength)).toString()));
		}

		/* Check that the commarea length can hold the input */
		if (commLength < dataLength) {
			throw (new CICWException(
					"Commarea length cannot be smaller than request length."
					+ " Commarea="
					+ (new Integer(commLength)).toString()
					+ ", request length="
					+ (new Integer(dataLength)).toString()));
		}

		/* Check that the commarea length can hold the input */
		if  (commLength < requestLength) {
			throw (new CICWException(
					"Too much data in request buffer. Expected="
					+ (new Integer(commLength)).toString()
					+ ", requested="
					+ (new Integer(requestLength)).toString()));
		}

	}
	
	/**
	 * Create and populate an HTTP post method to send the execution request.
	 * @param program the structure describing the program
	 * @param dataLength the size of the outbound data
	 * @param commLength the commarea length
	 * @param request the byte array containing the request data
	 * @return the new post method
	 */
	public final PostMethod createPostMethod(
			final CIXSProgram program,
			final int dataLength,
			final int commLength,
			final byte[] request) {
		
		int requestLength = (request == null) ? 0 : request.length;

		PostMethod postMethod = new PostMethod();

		/*  We don't want HTTPClient to retry if a 401 is received.
		 * (We include the user/password in the header on all requests so
		 *  we should not get the normal retry 401) */
		postMethod.setDoAuthentication(false);

		/* Point to CICW URL under CICS Web Support */ 
		postMethod.setPath(mHost.getHostCICWPath());

		/* Tell the CICS Web Server what program to run */ 
		postMethod.setRequestHeader("CICSProgram",
				program.getProgramName());

		/* Specifies the input data size */ 
		postMethod.setRequestHeader("CICSDataLength",
				new Integer(dataLength).toString());

		/* Tell the CICS Web Server what the commarea length should be and
		 * allocate a response buffer accordingly  */ 
		postMethod.setRequestHeader("CICSLength",
				new Integer(commLength).toString());

		/* See if there is actual data to send as the HTTP POST body */
		if (dataLength > 0) {
			/* When request buffer is exactly the input data, send it */
			if (dataLength == requestLength) {
				postMethod.setRequestEntity(
						new ByteArrayRequestEntity(request,
								BINARY_CONTENT_TYPE));
			} else {
				/* The request buffer has been allocated too large. We
				 * make sure we send only the useful data. This comes at
				 * the expense of another buffer allocation. */
				byte[] reducedRequest = new byte[dataLength];
				System.arraycopy(request, 0, reducedRequest, 0, dataLength);
				postMethod.setRequestEntity(
						new ByteArrayRequestEntity(reducedRequest,
								BINARY_CONTENT_TYPE));
			}
		}

		return postMethod;

	}

	/**
	 * Allocates a buffer and reads in the response coming back from the HTTP
	 * post method.
	 * @param postMethod the HTTP method object
	 * @param statusCode the status of the HTTP request
	 * @return a byte array containing the response
	 * @throws CICWException if response can't be retrieved
	 */
	public final byte[] getResponse(
			final PostMethod postMethod,
			final int statusCode) throws CICWException {

		byte[] response = null;
		InputStream respStream = null;

		/* Sanity check */
		if (postMethod.getResponseContentLength() > MAX_CONTENT_LEN) {
			throw (new CICWException("Response is larger than "
					+ MAX_CONTENT_LEN));
		}

		/* Nothing in the HTTP payload */
		if (postMethod.getResponseContentLength() == 0) {
			return response;
		}
		/* Try to get the response content  */
		try {
			respStream = postMethod.getResponseBodyAsStream();
		} catch (IOException e) {
			/* If this is an error situation anyway, don't throw an exception
			 * because it would hide the true error. */
			if (statusCode != HttpStatus.SC_OK) {
				return response;
			}
			e.printStackTrace();
			throw (new CICWException("IOException " + e.getMessage()));
		}

		/* Allocate the output buffer */
		response = new byte[(new Long(postMethod.
				getResponseContentLength())).intValue()];
		try {
			respStream.read(response);
			respStream.close();
		} catch (IOException e) {
			e.printStackTrace();
			throw (new CICWException("IOException " + e.getMessage()));
		}
		return response;
	}
	
	/**
	 * Try to send back the maximum amount of information to help the developer
	 * figure out what the problem is.
	 * @param postMethod the HTTP method that failed
	 * @param response the response content
	 * @throws CICWException the exception with meaningful message
	 */
	private void processError(
			final PostMethod postMethod,
			final byte[] response) throws CICWException {
		
		/* There might be more info on the specific error in HTTP
		 *  header CICSError */
		Header cicsError = postMethod.getResponseHeader("CICSError");
		if (cicsError != null) {
			throw (new CICWException(postMethod.getStatusText()
					+ " " + cicsError.getValue()));
		}
		
		/* There might be more info in the response data */
		if (response != null && response.length > 0) {
			try {
				String sb = new String(response,
						postMethod.getResponseCharSet());
				throw (new CICWException(postMethod.getStatusText()
						+ " " + sb));
			} catch (UnsupportedEncodingException e) {
				/* Better to pop the real exception */
				throw (new CICWException(postMethod.getStatusText()));
			}
			
		}
		
		/* All we seem to have is the HTTP status */
		throw (new CICWException(postMethod.getStatusText()));
	}

}

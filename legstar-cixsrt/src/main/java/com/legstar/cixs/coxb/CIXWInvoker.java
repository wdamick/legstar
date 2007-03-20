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
package com.legstar.cixs.coxb;

import java.io.IOException;
import java.util.logging.Logger;
import java.util.logging.Level;

import com.legstar.cixs.http.CICWConnection;
import com.legstar.cixs.http.CICWConnectionFactory;
import com.legstar.cixs.http.CICWException;
import com.legstar.cixs.http.CICWHost;
import com.legstar.coxb.CobolContext;
import com.legstar.coxb.CobolMarshalVisitor;
import com.legstar.coxb.CobolUnmarshalVisitor;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.host.HostException;
import com.legstar.coxb.convert.CobolConverters;

/**
 * This generic class has code to invoke a remote CICS program.
 * The input data buffer for the CICS program is built from a JAXB object tree 
 * and the reply is turned into a JAXB object tree.
 *
 * @author Fady Moussallam
 * 
 */
public class CIXWInvoker implements ICIXSInvoker {
	
	/** A properties file name giving host connectivity options. */
	private static final String HOST_PROP_FILE = "hostconnection.properties";
	
	/** Creates connections to a host. */
	private CICWConnection mConn;
	
	/** Describes the CICS host system. */
	private CICWHost mHost;

	/** Describes the CICS program to invoke. */
	private CIXSProgram mProgram;
	
    /** A logger conforming to the java common logging API. */
	private final Logger mLogger =
    	Logger.getLogger(this.getClass().getPackage().getName());
	
	/**
	 * Contructor loads a host connectivity options properties file.
	 * @throws CIXSException if loading host connectivity options fails
	 */
	public CIXWInvoker() throws CIXSException {
		try {
			mHost = new CICWHost(HOST_PROP_FILE);
		} catch (IOException e) {
			e.printStackTrace();
			mLogger.throwing(this.getClass().toString(),
					"Error reading " + HOST_PROP_FILE + " file", e);
			throw (new CIXSException("IOException " + e.getMessage()));
		}
	}
	
	/** {@inheritDoc} */
	public final void initialize(
			final CIXSHeader header,
			final String operationName) throws CIXSException {
		
		try {
			mProgram = new CIXSProgram(operationName + ".properties");
		} catch (IOException e) {
			e.printStackTrace();
			mLogger.throwing(this.getClass().toString(),
					"Error reading " + operationName + ".properties file", e);
			throw (new CIXSException(e.getMessage()));
		}
		/* Propagate header data on host connectivity parameters */
		propagateHeaderData(header);
		
		CICWConnectionFactory cCF = new CICWConnectionFactory();
		mConn = cCF.createConnection(mHost);
	}
	

	/** {@inheritDoc} */
	public final CIXSParameter createParameter() {
		/* Create a cobol context with data from hostconnection.properties*/ 
		CobolContext cobolContext = new CobolContext();
		if (mHost != null) {
			cobolContext.setHostCharsetName(mHost.getHostCharsetName());
		}
		/* Select a conversion strategy */ 
		CobolConverters cobolConverters =
			new CobolSimpleConverters(cobolContext);
		return new CIXSParameter(cobolConverters);
	}

	/** {@inheritDoc} */
	public final void invoke(
			final CIXSParameter inParameter,
			final CIXSParameter outParameter) throws CIXSException {
		
		if (mConn == null) {
    		throw new CIXSException("You must initialize first");
    	}
    	
		long start = System.currentTimeMillis();
		mLogger.entering(this.getClass().toString(),
				"invoke started for program " + mProgram.getProgramName());

		byte[] hostRequest;
		try {
			/* Allocate an populate a host data buffer for the request */
			hostRequest = marshal(inParameter);

			/* With the host data buffer we can now invoke the CICS program */
			byte[] hostReply = mConn.invoke(mProgram, hostRequest);
			
			/* Unmarshall the raw host reply into a java object tree */
			unmarshal(hostReply, outParameter);
		} catch (CICWException e) {
			e.printStackTrace();
			throw (new CIXSException(e.getMessage()));
		}
		
		
		long end = System.currentTimeMillis();
		mLogger.exiting(this.getClass().toString(), "invoke ended "
				+ (new Long(end - start)).toString() + " ms");
		
		return;
	}
	
	/**
	 * Convert JAXB object tree to a host buffer.
	 * 
	 * @param inParameter input parameters description
	 * @return the host buffer
	 * @throws CICWException if call fails
	 */
	private byte[] marshal(
			final CIXSParameter inParameter) throws CICWException {

    	long start = System.currentTimeMillis();
		mLogger.entering(this.getClass().toString(), "marshal started");

		/* Allocate a host data buffer for the request */
		byte[] hostRequest = new byte[mProgram.getCommareaLength()];
		
		/* create the outbound buffer by marshalling the java object tree */
		CobolMarshalVisitor mv = new CobolMarshalVisitor(
				hostRequest, 0, inParameter.getCobolConverters());
		try {
			/** Traverse the object structure, visiting each node with the
			 *  visitor */
			inParameter.getComplexBinding().accept(mv);
		} catch (HostException he) {
			mLogger.log(Level.SEVERE, "Host marshaling failed.", he);
			throw (new CICWException("Marshalling error:" + he.getMessage()));
		}
		/* Adjust the input length depending on the real input buffer size */
		mProgram.setDataLength(mv.getOffset());
 
		long end = System.currentTimeMillis();
		mLogger.exiting(this.getClass().toString(), "marshal ended "
				+ (new Long(mv.getOffset())).toString() + " bytes "
				+ (new Long(end - start)).toString() + " ms");
		
		return hostRequest;
	}

	/**
	 * Convert a host buffer to a JAXB object tree.
	 * 
	 * @param hostReply the host data buffer
	 * @param outParameter output parameters description
	 * @throws CICWException if call fails
	 */
	private void unmarshal(
			final byte[] hostReply,
			final CIXSParameter outParameter) throws CICWException {

    	long start = System.currentTimeMillis();
		mLogger.entering(this.getClass().toString(), "unmarshal started");

		/* Unmarshall the raw host reply into a java object tree */
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(
				hostReply, 0, outParameter.getCobolConverters());
		try {
			/** Traverse the object structure, visiting each node with the
			 *  visitor */
			outParameter.getComplexBinding().accept(uv);
		} catch (HostException he) {
			mLogger.log(Level.SEVERE, "Host unmarshaling failed.", he);
			throw (new CICWException("Unmarshalling error:" + he.getMessage()));
		}

		long end = System.currentTimeMillis();
		mLogger.exiting(this.getClass().toString(), "unmarshal ended "
				+ (new Long(uv.getOffset())).toString() + " bytes "
				+ (new Long(end - start)).toString() + " ms");
		
	}

    /**
     * Propagates header data received to the corresponding host connectivity
     * parameter.
     * @param header the header data coming from the client
     */
    private void propagateHeaderData(final CIXSHeader header) {
    	
    	if (header == null) {
    		return;
    	}
    	
		if (header.getHostUser() != null 
				&& header.getHostUser().length() > 0) {
			mHost.setHostUser(header.getHostUser());
		}
		if (header.getHostPassword() != null 
				&& header.getHostPassword().length() > 0) {
			mHost.setHostPassword(header.getHostPassword());
		}
		if (header.getHostIPAddress() != null 
				&& header.getHostIPAddress().length() > 0) {
			mHost.setHostIPAddress(header.getHostIPAddress());
		}
		if (header.getHostIPPort() > 0) {
			mHost.setHostIPPort(header.getHostIPPort());
		}
		if (header.getHostCICWPath() != null 
				&& header.getHostCICWPath().length() > 0) {
			mHost.setHostCICWPath(header.getHostCICWPath());
		}
    }
	/**
	 * @return Returns the Connection object.
	 */
	public final CICWConnection getConn() {
		return mConn;
	}

	/**
	 * @return Returns the Host connection descriptor.
	 */
	public final CICWHost getHost() {
		return mHost;
	}

	/**
	 * @return Returns the Program structure.
	 */
	public final CIXSProgram getProgram() {
		return mProgram;
	}
}

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
package com.legstar.messaging.impl;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.visitor.CobolMarshalVisitor;
import com.legstar.coxb.visitor.CobolUnmarshalVisitor;
import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.convert.CobolConverters;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.host.HostException;

/**
 * Provide Cobol marshaling/unmarshaling static methods.
 */
public final class CobolTransformer {

	/** Logger. */
	private static final Log LOG = LogFactory.getLog(CobolTransformer.class);
	
	/**
	 * This is a utility class not meant for instantiation.
	 */
	private CobolTransformer() {
	}
	
	/**
	 * Convert JAXB object tree to a host buffer.
	 * 
	 * @param cobolConverters a conversion strategy
	 * @param ccbin input object tree
	 * @param hostInputBytes a byte buffer which will be filled with host data
	 * @return the actual number of bytes marshaled
	 * @throws HostMarshalException if marshaling failed
	 */
	public static int marshal(
			final CobolConverters cobolConverters,
			final ICobolComplexBinding ccbin,
			final byte[] hostInputBytes) throws HostMarshalException {

		int bytesMarshalled = 0;
		long start = System.currentTimeMillis();
		if (LOG.isDebugEnabled()) {
 			LOG.debug("Marshaling started");
		}

		/* create the outbound buffer by marshalling the java object tree */
		CobolMarshalVisitor mv =
			new CobolMarshalVisitor(hostInputBytes, 0, cobolConverters);
		try {
			/* Traverse the object structure, visiting each node with the
			 *  visitor */
			ccbin.accept(mv);
		} catch (HostException he) {
			throw new HostMarshalException(he);
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
	 * @param cobolConverters a conversion strategy
	 * @param hostOutputBytes the host data buffer
	 * @param ccbout output object tree
	 * @throws HostUnmarshalException if unmarshaling failed
	 */
	public static void unmarshal(
			final CobolConverters cobolConverters,
			final byte[] hostOutputBytes,
			final ICobolComplexBinding ccbout) throws HostUnmarshalException {

		int bytesUnmarshalled = 0;
    	long start = System.currentTimeMillis();
		if (LOG.isDebugEnabled()) {
			LOG.debug("Unmarshaling started");
		}

		/* Unmarshal the raw host reply into a java object tree */
		CobolUnmarshalVisitor uv = new CobolUnmarshalVisitor(
				hostOutputBytes, 0, cobolConverters);
		try {
			/* Traverse the object structure, visiting each node with the
			 *  visitor */
			ccbout.accept(uv);
		} catch (HostException he) {
			throw new HostUnmarshalException(he);
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
     * Creates a simple set of COBOl converters.
     * @param hostCharset the host character set
     * @return a set of COBOL converters
     */
    public static CobolConverters getCobolConverters(final String hostCharset) {
    
        /* Setup cobol converters with the target character set */
        CobolContext cobolContext = new CobolContext();
        cobolContext.setHostCharsetName(hostCharset);
        
        /* Select a conversion strategy */ 
        return new CobolSimpleConverters(cobolContext);
    }
}

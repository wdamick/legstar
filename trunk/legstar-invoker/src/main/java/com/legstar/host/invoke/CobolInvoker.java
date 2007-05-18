package com.legstar.host.invoke;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.CobolMarshalVisitor;
import com.legstar.coxb.CobolUnmarshalVisitor;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.convert.CobolConverters;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.host.HostException;

/**
 * All invokers that use Cobol marshaling/unmarshaling  can derive from
 * this class.
 */
public abstract class CobolInvoker {

	/** Cobol to Java converters. */
	private CobolConverters mCobolConverters;
	
	/** Logger. */
	private static final Log LOG = LogFactory.getLog(CobolInvoker.class);
	
	/**
	 * In this version, the constructor uses a set of simple Cobol converters.
	 * @param hostCharset the host character set
	 */
	public CobolInvoker(final String hostCharset) {

		/* Setup cobol converters with the target character set */
		CobolContext cobolContext = new CobolContext();
		cobolContext.setHostCharsetName(hostCharset);
		
		/* Select a conversion strategy */ 
		mCobolConverters = new CobolSimpleConverters(cobolContext);
		
	}
	
	/**
	 * Convert JAXB object tree to a host buffer.
	 * 
	 * @param ccbin input object tree
	 * @param hostInputBytes a byte buffer which will be filled with host data
	 * @return the actual number of bytes marshaled
	 * @throws MarshalException if marshaling failed
	 */
	public final int marshal(
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
	public final void unmarshal(
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
	 * @return the cobol converters
	 */
	public final CobolConverters getCobolConverters() {
		return mCobolConverters;
	}


}

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
package com.legstar.csok.client;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * This class is used to accumulate small chunks of data before they are
 * actually written to the target output stream. This is useful when the
 * output stream behaves better with few large chunks of data rather than
 * many small ones.
 *
 */
public class CicsSocketOutputBuffer {
	
	/** Fixed buffer size. */
	private int mOutputBufferLen;
	
	/** Buffer used to accumulate data before it is flushed. */
	private byte[] mOutputBuffer;
	
	/** Current position within the buffer. */
	private int mPos;
	
	/** The output stream used to flush data when buffer is full. */
	private OutputStream mOutputStream;
	
	/**
	 * Creates a buffered output stream.
	 * @param out an output stream
	 * @param bufferLen the output buffer byte length
	 */
	public CicsSocketOutputBuffer(
			final OutputStream  out, final int bufferLen) {
		mOutputStream = out;
		mOutputBufferLen = bufferLen;
		mOutputBuffer = new byte[bufferLen];
		mPos = 0;
	}
	
	/**
	 * Writes data into the buffer if it fits.
	 * @param buffer the buffer containing the data to send
	 * @throws IOException if write fails
	 */
	public final  void write(final byte[] buffer) throws IOException {
		write(buffer, 0, buffer.length);
	}
	
	/**
	 * Writes data into the buffer if it fits.
	 * @param buffer the buffer containing the data to send
	 * @param len the size in bytes of the data to send
	 * @throws IOException if write fails
	 */
	public final void write(
			final byte[] buffer, final int len) throws IOException {
		write(buffer, 0, len);
	}
	
	/**
	 * Writes data into the buffer if it fits.
	 * @param buffer the buffer containing the data to send
	 * @param off the offset within buffer where data starts
	 * @param len the size in bytes of the data to send
	 * @throws IOException if write fails
	 */
	public final  void write(
			final byte[] buffer,
			final int off,
			final int len) throws IOException {
	    /* If the requested data is larger than the send buffer, we just
	     * ignore the buffering request. Since data is expected to be
	     * sent in order, we first flush any pending buffered data */
		if (len > mOutputBufferLen) {
			flush();
			mOutputStream.write(buffer, off, len);
			return;
		}
		
	    /* If the data to send is larger than the free space in the send
	     * buffer, flush the send buffer first to make room. */
		if (len > (mOutputBufferLen - mPos)) {
			flush();
			write(buffer, off, len);
			return;
		}

		/* At this stage, the data fits into the buffer so just copy it  */
		System.arraycopy(buffer, off, mOutputBuffer, mPos, len);
		mPos += len;
	}
	
	/**
	 * When reading from a stream, this results in a dual buffering situation
	 * where data is first read into a local buffer and then copied to the
	 * general buffer before it is actually flushed to the socket.
	 * @param inStream the input stream
	 * @throws IOException if reading from the input stream fails
	 */
	public final void write(final InputStream inStream) throws IOException {
		byte[] buffer = new byte[1024];
		int rc;
		while ((rc = inStream.read(buffer)) > 0) {
			write(buffer, 0, rc);
		}
	}
	
	/**
	 * Sends all pending buffered data on the socket stream.
	 * @throws IOException if send fails
	 */
	public final  void flush() throws IOException {
		if (mPos > 0) {
			mOutputStream.write(mOutputBuffer, 0, mPos);
			mPos = 0;
		}
	}

}

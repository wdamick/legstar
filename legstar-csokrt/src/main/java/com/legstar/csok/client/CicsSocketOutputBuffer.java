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
package com.legstar.csok.client;

import java.io.IOException;
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

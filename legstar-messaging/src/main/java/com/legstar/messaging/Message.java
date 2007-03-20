package com.legstar.messaging;

import java.util.List;

/**
 * Messages represents the input and output of requests. A message is composed
 * of message parts: one header part and any number of data message parts.
 */
public class Message {
	
	/** Header message part. */
	private HeaderPart mHeaderPart;
	
	/** Data message parts. */
	private List < MessagePart > mDataParts;

	/**
	 * Construct a message from its message parts.
	 * @param headerPart the header message part
	 * @param dataParts the data message parts
	 */
	public Message(
			final HeaderPart headerPart,
			final List < MessagePart > dataParts) {
		mHeaderPart = headerPart;
		mDataParts = dataParts;
	}

	/**
	 * @return the list of data message parts
	 */
	public final List < MessagePart > getDataParts() {
		return mDataParts;
	}
	
	/**
	 * @param dataParts the list of data message parts to set
	 */
	public final void setDataParts(final List < MessagePart > dataParts) {
		mDataParts = dataParts;
	}

	/**
	 * @return the header message part
	 */
	public final HeaderPart getHeaderPart() {
		return mHeaderPart;
	}

	/**
	 * @param headerPart the header message part to set
	 */
	public final void setHeaderPart(final HeaderPart headerPart) {
		mHeaderPart = headerPart;
	}

}

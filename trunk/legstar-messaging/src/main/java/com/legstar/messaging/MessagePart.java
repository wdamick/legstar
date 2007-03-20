package com.legstar.messaging;

/**
 * MessageParts are generic named, binary containers used to send
 * and receive from host.
 */
public class MessagePart {
	
	/** The message part identifier. */
	private String mID;
	
	/** The message content. */
	private byte[] mContent;
	
	/**
	 * Create a named message part from a content.
	 * @param id the message part identifier
	 * @param content a binary content
	 */
	public MessagePart(final String id, final byte[] content) {
		mID = id;
		mContent = content;
	}

	/**
	 * @return the message content
	 */
	public final byte[] getContent() {
		return mContent;
	}

	/**
	 * @param content the message content to set
	 */
	public final void setContent(final byte[] content) {
		mContent = content;
	}

	/**
	 * @return the message part identifier
	 */
	public final String getID() {
		return mID;
	}

	/**
	 * @param id the message part identifier to set
	 */
	public final void setID(final String id) {
		mID = id;
	}

}

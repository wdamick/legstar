package com.legstar.messaging;


/**
 * MessagePart representing the content of a CICS commarea.
 */
public class CommareaPart extends MessagePart {

	/** The header identifier (used as an eye catcher by the CICS
	 *  counterpart).*/
	public static final String COMMAREA_PART_ID = "LSOKCOMMAREA";
	
	/**
	 * Create a commarea from a binary content.
	 * @param content binary content
	 */
	public CommareaPart(final byte[] content) {
		super(COMMAREA_PART_ID, content);
	}

}

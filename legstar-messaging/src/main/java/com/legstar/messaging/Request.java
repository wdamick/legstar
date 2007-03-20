package com.legstar.messaging;

/**
 * This class represents a request for its entire lifetime. It is created
 * by a client with input data and then processed by the server which
 * attaches reply data or an exception as appropriate.
 */
public class Request {
	
	/** A unique ID for this request. */
	private String mID;
	
	/** The request message. */
	private Message mRequestMessage;
	
	/** The response message. */
	private Message mResponseMessage;
	
	/** If request processing failed, this will describe the failure. */
	private Exception mException;
	
	/** Host destination for this request. */
	private Address mAddress;
	
	/** Creates an empty request. */
	public Request() {
	}
	
	/**
	 * Constructor for a new request.
	 * @param id a unique ID for this request
	 * @param address the host destination
	 * @param requestMessage the input message
	 */
	public Request(
			final String id,
			final Address address,
			final Message requestMessage) {
		mID = id;
		mAddress = address;
		mRequestMessage = requestMessage;
	}

	/**
	 * @return the unique ID for this request
	 */
	public final String getID() {
		return mID;
	}

	/**
	 * @param id the unique ID for this request to set
	 */
	public final void setID(final String id) {
		mID = id;
	}

	/**
	 * @return the the input message
	 */
	public final Message getRequestMessage() {
		return mRequestMessage;
	}

	/**
	 * @param requestMessage the input message to set
	 */
	public final void setRequestMessage(final Message requestMessage) {
		mRequestMessage = requestMessage;
	}

	/**
	 * @return the reply message
	 */
	public final Message getResponseMessage() {
		return mResponseMessage;
	}

	/**
	 * @param responseMessage the reply message to set
	 */
	public final void setResponseMessage(final Message responseMessage) {
		mResponseMessage = responseMessage;
	}

	/**
	 * @return the work exception
	 */
	public final Exception getException() {
		return mException;
	}

	/**
	 * @param requestException the work exception to set
	 */
	public final void setException(final Exception requestException) {
		mException = requestException;
	}

	/**
	 * @return the host destination
	 */
	public final Address getAddress() {
		return mAddress;
	}

	/**
	 * @param address the host destination to set
	 */
	public final void setAddress(final Address address) {
		mAddress = address;
	}

}

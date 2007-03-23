package com.legstar.csok.client;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.logging.Log; 
import org.apache.commons.logging.LogFactory; 

import com.legstar.messaging.Connection;
import com.legstar.messaging.ConnectionException;
import com.legstar.messaging.HeaderPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.Message;
import com.legstar.messaging.MessagePart;
import com.legstar.messaging.Request;
import com.legstar.messaging.RequestException;


/**
 * Client side CICS Socket connectivity. This class provides the core
 * methods to connect to CICS over sockets, send requests, receive 
 * results, etc...
 */
public class CicsSocket implements Connection {
	
	/** Length of the Transaction Initial Message expected by the IBM CICS
	 *  Socket listener. */
	private static final int CIM_LEN = 35;
	
	/** Length of a message part header. */
	private static final int MPH_LEN = 29;
	
	/** Size of the CICS USER ID. */
	private static final int CICS_USERID_LEN = 8;

	/** Size of the CICS Password. */
	private static final int CICS_PWD_LEN = 8;
	
	/** Size of connection identifier. */
	private static final int CONNECTION_ID_LEN = 16;
	
	/** Size of message type. */
	private static final int MSG_TYPE_LEN = 9;
	
	/** Size of message part identifier. */
	private static final int MSG_PART_ID_LEN = 16;
	
	/** Size of header preceding any reply from host. */
	private static final int REPLY_HDR_LEN = 9;
	
	/** Size of trace option. */
	private static final int TRACE_LEN = 1;
	
	/** Maximum size of the host reply for acknowledgements and error
	 *  reports. */
	private static final int MAX_PROT_REPLY_LEN = 266;
	
	
	/** CIM eye catcher. */
	private static final String CIM_EYE_CATCHER = "SK";
	
	/** Identifies execution requests. */
	private static final String EXEC_REQUEST_EC = "LSOKEXEC";
	
	/** Identifies probes (is alive ). */
	private static final String PROBE_REQUEST_EC = "LSOKPROB";
	
	/** Reply eye catcher for acknowledgements. */
	private static final String REPLY_ACK_MSG_EC = "LSOKACK0";
	
	/** Reply eye catcher for errors. */
	private static final String REPLY_ERROR_MSG_EC = "LSOKERR0";
	
	/** Eye catcher for data messages. */
	private static final String DATA_MSG_EC = "LSOKDATA";
	
	/** Processing instructions for UOW handling. */
	private static final String UOW_MSG_EC = "LSOKUOWC";
	
	/** UOW processing msg eye catcher length. */
	private static final int UOW_MSG_EC_LEN = 8;
	
	/** UOW command length. */
	private static final int UOW_COMMAND_LEN = 8;
	
	/** Processing instructions for UOW commit. */
	private static final String UOW_COMMIT = "Commit";
	
	/** Processing instructions for UOW rollback. */
	private static final String UOW_ROLLBACK = "Rollback";
	
	/** Processing instructions for UOW keep. */
	private static final String UOW_KEEP = "Keep";
	
	/** General protocol violation reply message. */
	private static final String PROTOCOL_ERROR_MSG =
		"Invalid or unexpected reply from host.";
	
	/** TCPIP Socket connection to a CICS region. */
	private Socket mClientSocket;
	
	/** An identifier for this connection. */
	private String mConnectionID;
	
	/** Host CICS Socket endpoint. */
	private CicsSocketEndpoint mCicsSocketEndpoint;
	
	/** Because it is used throughout this class, the charset is 
	 * stored once and for all rather than retrieved from endpoint.*/
	private String mHostCharset;
	
	/** maximum time (milliseconds) to wait for connection. */
	private int mConnectTimeout;
	
	/** maximum time (milliseconds) to wait for host reply. */
	private int mReceiveTimeout;
	
	/** Logger. */
	private static final Log LOG = LogFactory.getLog(CicsSocket.class);
	
	/**
	 * A CICS Socket exists for a target CICS region, a given CICS Socket
	 * listener and a particular User ID to use for authentication and 
	 * impersonation. Observe that no password is stored in this class
	 * for security reasons.
	 * 
	 * @param connectionID an identifier for this connection
	 * @param cicsSocketEndpoint CICS Socket endpoint
	 * @param connectionTimeout Maximum time (milliseconds) to wait for
	 *  connection
	 * @param receiveTimeout Maximum time (milliseconds) to wait for host reply
	 */
	public CicsSocket(
			final String connectionID,
			final CicsSocketEndpoint cicsSocketEndpoint,
			final int connectionTimeout,
			final int receiveTimeout) {
		mConnectionID = connectionID;
		mConnectTimeout = connectionTimeout;
		mReceiveTimeout = receiveTimeout;
		mCicsSocketEndpoint = cicsSocketEndpoint;
		mHostCharset = cicsSocketEndpoint.getHostCharset();
	}
	
	/**
	 * Connect to a CICS IBM Listener passing credentials and a client
	 * initial message. The reply must be an acknowldgement otherwise it
	 * is an error message returned from the host.
	 * 
	 * @param cicsPassword credentials for security exist
	 * @throws ConnectionException if connection fails
	 */
	 public final void connect(
			 final String cicsPassword) throws ConnectionException {
		String password;
		if (LOG.isDebugEnabled()) {
			LOG.debug("Connection:" + mConnectionID
					 + " Attempting connection. Host:" 
					 + mCicsSocketEndpoint.getReport());
		}
		try {
			mClientSocket = new Socket();
			mClientSocket.connect(
					new InetSocketAddress(
							mCicsSocketEndpoint.getHostIPAddress(),
							mCicsSocketEndpoint.getHostIPPort()),
							mConnectTimeout);
			mClientSocket.setSoTimeout(mReceiveTimeout);
		    /* In order to optimize memory allocation, this client program
		     * sends message parts to server in 2 different sends. If
		     * we don t disable Nagle, there is an unacceptable delay in
		     * the server acknowldgement of the first send. */
			mClientSocket.setTcpNoDelay(true);
			
			/* If a password is not passed, use the one from configuration */
			if (cicsPassword == null || cicsPassword.length() == 0) {
				password = mCicsSocketEndpoint.getHostPassword();
			} else {
				password = cicsPassword;
			}
			
			OutputStream  out = mClientSocket.getOutputStream();
			out.write(formatCIM(
					mCicsSocketEndpoint.getHostUserID(),
					password,
					mConnectionID,
					mCicsSocketEndpoint.isHostTraceMode(),
					mHostCharset));
			recvConnectionAck();
		} catch (UnknownHostException e) {
			throw (new ConnectionException(e));
		} catch (IOException e) {
			throw (new ConnectionException(e));
		} catch (CicsSocketHostConversionException e) {
			throw (new ConnectionException(e));
		} catch (RequestException e) {
			throw (new ConnectionException(e));
		}
		if (LOG.isDebugEnabled()) {
			LOG.debug("Connection:" + mConnectionID + " Connected.");
		}
	}
	
	/**
	 * This method probes a socket for reusability.
	 * A socket might be reusable (which is efficient). Unfortunatly,
	 * there are no socket methods that reliably tells you if socket
	 * is usable or not.
	 * @param cicsPassword host password if it is not stored in configuration
	 *  file
	 *  @throws ConnectionException if connection fails
	 * */
	public final void connectReuse(
			final String cicsPassword) throws ConnectionException {
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("Connection:" + mConnectionID + " Attempting reuse.");
		}
		/* If socket is reusable just return. */
		if (mClientSocket != null
				&& mClientSocket.isConnected() && isServerAlive()) {
			if (LOG.isDebugEnabled()) {
				LOG.debug("Connection:" + mConnectionID
						+ " Socket will be reused.");
			}
			return;
		}
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("Connection:" + mConnectionID + " Socket not reusable.");
		}
		/* Socket is not reusable, fallback to standard connect. */
		connect(cicsPassword);
	}
	
	/**
	 * After initial connect, the host should reply with an ack plus a 
	 * bunch of attributes used to correlate this connection with host
	 * events.
	 * 
	 * @throws RequestException if ack cannot be received
	 */
	private void recvConnectionAck() throws RequestException {
		String ackString = null;
		try {
			InputStream  in = mClientSocket.getInputStream();
			ackString = getCharsFromHost(
					in, mHostCharset, MAX_PROT_REPLY_LEN);
			/* If this is not a valid ACK, it must be an error report*/
			if (REPLY_ACK_MSG_EC.compareTo(
					ackString.substring(
							0, REPLY_ACK_MSG_EC.length())) != 0) {
				throw (new RequestException(ackString));
			}
		} catch (IOException e) {
			throw (new RequestException(e));
		}
		if (LOG.isDebugEnabled()) {
			LOG.debug("Connection:" + mConnectionID
					+ " Connection Ack received."
					+ ackString);
		}
	}
	
	/**
	 * A request is serialized as a header message part followed by 
	 * data message parts.
	 * 
	 * @param request the request to be serviced
	 * @throws RequestException if send fails
	 */
	public final void sendRequest(
			final Request request) throws RequestException {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Sending Request:" + request.getID()
			   + " on Connection:" + mConnectionID
			   + " "
			   + request.getRequestMessage().getHeaderPart().
			   			getStringizedKeyValues()
			   + '.');
		}
		CicsSocketOutputBuffer outputBuffer;
		try {
			/* Buffer output to reduce the number of individual socket sends.*/
			outputBuffer = new CicsSocketOutputBuffer(
					mClientSocket.getOutputStream(),
					mClientSocket.getSendBufferSize());
			sendMessagePart(EXEC_REQUEST_EC, request.getID(),
					(MessagePart) request.getRequestMessage().getHeaderPart(),
					outputBuffer);
			Iterator < MessagePart > entries =
				request.getRequestMessage().getDataParts().iterator();
			while (entries.hasNext()) {
				sendMessagePart(DATA_MSG_EC, request.getID(), entries.next(),
						outputBuffer);
			}
			outputBuffer.flush();
		} catch (IOException e) {
			throw (new RequestException(e));
		}

		if (LOG.isDebugEnabled()) {
			LOG.debug("Request:" + request.getID()
					   + " on Connection:" + mConnectionID
					   + " message request sent.");
		}
	}
	
	/**
	 * Send a message part to the host.
	 * A message part is serialized this way:
	 * - 9 chars message type
	 * - 16 chars ID of the message part
	 * - 4 bytes, length of content (big endian int)
	 * - Content
	 * @param messageType message part type
	 * @param requestID the request identifier
	 * @param message the message part
	 * @param outputBuffer a buffered output stream
	 * @throws RequestException if send fails
	 */
	public final void sendMessagePart(
			final String messageType,
			final String requestID,
			final MessagePart message,
			final CicsSocketOutputBuffer outputBuffer) throws RequestException {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Request:" + requestID
					   + " on Connection:" + mConnectionID
					   + " sending message part:"
					   + message.getID()
					   + " size:"
					   + new Integer((message.getContent() == null)
							   ? 0 : message.getContent().length).toString()
					   + '.');
		}
		try {
			outputBuffer.write(formatMPH(messageType, message, mHostCharset));
			if (message.getContent() != null
					&& message.getContent().length > 0) {
				outputBuffer.write(message.getContent());
			}
		} catch (IOException e) {
			throw (new RequestException(e));
		} catch (CicsSocketHostConversionException e) {
			throw (new RequestException(e));
		}
		if (LOG.isDebugEnabled()) {
			LOG.debug("Request:" + requestID
					   + " on Connection:" + mConnectionID
					   + " message part sent.");
		}
	}
	
	
	/**
	 * A response is serialized as a header message part followed by 
	 * data message parts. This method creates a response message
	 * for the request.
	 * 
	 * @param request the request being serviced
	 * @throws RequestException if receive fails
	 */
	public final void recvResponse(
			final Request request) throws RequestException {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Receiving response for Request:" + request.getID()
					   + " on Connection:" + mConnectionID
					   + '.');
		}
		MessagePart responseHeader = recvMessagePart(request.getID());
		HeaderPart headerPart;
		try {
			headerPart = new HeaderPart(responseHeader);
		} catch (HeaderPartException e) {
			throw new RequestException(e);
		}
		List < MessagePart > dataParts = new ArrayList < MessagePart >();
		for (int i = 0; i < headerPart.getDataPartsNumber(); i++) {
			dataParts.add(recvMessagePart(request.getID()));
		}
		request.setResponseMessage(new Message(headerPart, dataParts));
		if (LOG.isDebugEnabled()) {
			LOG.debug("Request:" + request.getID()
					   + " on Connection:" + mConnectionID
					   + " response received.");
		}
	}
	
	/**
	 * Expecting reply from host after a request was acknowledged.
	 * 
	 * @param requestID the request identifier
	 * @return the message part received
	 * @throws RequestException if receive operation fails
	 */
	public final MessagePart recvMessagePart(
			final String requestID) throws RequestException {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Receiving a message part for Request:" + requestID
					   + " on Connection:" + mConnectionID
					   + '.');
		}
		MessagePart reply = null;
		try {
			/* First get the eye catcher portion of the reply */
			InputStream  in = mClientSocket.getInputStream();
			String msgType = recvMessageType(in);
			
			/* Check if this is a valid reply or an error reply */
			if (DATA_MSG_EC.compareTo(
					msgType.substring(
							0, DATA_MSG_EC.length())) == 0) {
				reply = recvMessagePart(requestID, in);
			} else {
				recvError(msgType, in);
			}
		} catch (IOException e) {
			throw (new RequestException(e));
		}
		if (LOG.isDebugEnabled()) {
			LOG.debug("Request:" + requestID
				   + " on Connection:" + mConnectionID
				   + " message part received:"
				   + ((reply == null) ? "null" : reply.getID()
						   + " size:"
						   + new Integer((reply.getContent() == null)
							? 0 : reply.getContent().length).toString())
				   + '.');
		}
		return reply;
	}
	
	/**
	 * All replies must start with a fixed size message type.
	 * @param in an opened input stream on the host
	 * @return the header element
	 * @throws RequestException if header element cannot be recovered
	 */
	private String recvMessageType(
			final InputStream  in) throws RequestException {
		String msgType = getCharsFromHost(
				in, mHostCharset, REPLY_HDR_LEN);
		if (msgType == null || msgType.length() < REPLY_HDR_LEN) {
			throw (new RequestException(PROTOCOL_ERROR_MSG));
		}
		return msgType;
	}
	
	/**
	 * When the reply does not present the expected header, it
	 * might contain an error report.
	 * @param msgType the header received
	 * @param in an opened input stream on the host
	 * @throws RequestException in all cases
	 */
	private void recvError(
			final String msgType,
			final InputStream  in) throws RequestException {

		if (REPLY_ERROR_MSG_EC.compareTo(
				msgType.substring(
						0, REPLY_ERROR_MSG_EC.length())) != 0) {
			/* Consider this is a system error message */
			String errString = getCharsFromHost(
					in, mHostCharset, MAX_PROT_REPLY_LEN);
			throw (new RequestException(msgType + errString));
		} else {
			/* Get the error message content */
			String errString = getCharsFromHost(
					in, mHostCharset, MAX_PROT_REPLY_LEN);
			throw (new RequestException(errString.trim()));
		}
	}
	
	/**
	 * Retrieve a message part in the reply coming back from the host.
	 * @param requestID the request identifier
	 * @param in an opened input stream on the host
	 * @return the message part received
	 * @throws RequestException if receive operation fails
	 */
	private MessagePart recvMessagePart(
			final String requestID,
			final InputStream  in) throws RequestException {
		
		/* The message part ID  */
		String idString = getCharsFromHost(
				in, mHostCharset, MSG_PART_ID_LEN).trim();
		
		/* Receive the content size */
		int size = 4;
		byte[] sizeBytes = receiveSize(in, size);
		
		/* Convert content size from bytes to int*/
		ByteBuffer bb = ByteBuffer.allocate(size);
		bb.put(sizeBytes);
		bb.flip();
		size = bb.asIntBuffer().get();

		/* Receive the content */
		byte[] content = receiveSize(in, size);
		
		return new MessagePart(idString, content);
	}
	
	/**
	 * Terminates a connection with the host.
	 * @throws RequestException if a failure is detected
	 */
	public final void close() throws RequestException {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Connection:" + mConnectionID
					+ " Attempting to close.");
		}
		if (mClientSocket == null) {
			return;
		}
		if (!mClientSocket.isClosed()) {
			try {
				mClientSocket.getInputStream().close();
				mClientSocket.close();
			} catch (IOException e) {
				throw (new RequestException(e));
			}
		}
		mClientSocket = null;
		if (LOG.isDebugEnabled()) {
			LOG.debug("Connection:" + mConnectionID + " Closed.");
		}
	}
	
	/**
	 * Request Unit Of Work commit.
	 * @throws RequestException if a failure is detected
	 */
	public final void commitUOW() throws RequestException {
		processUOW(UOW_COMMIT);
	}
	
	/**
	 * Request Unit Of Work continuation.
	 * @throws RequestException if a failure is detected
	 */
	public final void keepUOW() throws RequestException {
		processUOW(UOW_KEEP);
	}
	
	/**
	 * Request Unit Of Work rollback.
	 * @throws RequestException if a failure is detected
	 */
	public final void rollbackUOW() throws RequestException {
		processUOW(UOW_ROLLBACK);
	}
	
	/**
	 * Instruct host on Unit Of Work processing and wait for Ack.
	 * @param command the unit of work command (commit, rollback or keep)
	 * @throws RequestException if a failure is detected
	 */
	private void processUOW(
			final String command) throws RequestException {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Connection:" + mConnectionID + " Attempting to "
				    + command + " unit of work.");
		}
		try {
			OutputStream  out = mClientSocket.getOutputStream();
			out.write(formatUOW(command, mHostCharset));
			receiveAck();
		} catch (IOException e) {
			throw (new RequestException(e));
		} catch (CicsSocketHostConversionException e) {
			throw (new RequestException(e));
		}
		if (LOG.isDebugEnabled()) {
			LOG.debug("Connection:" + mConnectionID + " "
				    + command + " success.");
		}
	}
	
	/**
	 * Used to check if server is still alive.
	 * @return true if server transaction is alive
	 */
	private boolean isServerAlive() {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Connection:" + mConnectionID
					+ " Attempting to probe server.");
		}
		try {
			OutputStream  out = mClientSocket.getOutputStream();
			out.write(formatProbe(mHostCharset));
			receiveAck();
			return true;
		} catch (IOException e) {
			return false;
		} catch (CicsSocketHostConversionException e) {
			return false;
		} catch (RequestException e) {
			return false;
		}
	}
	
	/**
	 * Expecting an aknowlegement coming back from the host in reply to
	 * something we just sent.
	 * 
	 * @throws RequestException if ack cannot be received
	 */
	private void receiveAck() throws RequestException {
		try {
			InputStream  in = mClientSocket.getInputStream();
			String msgType = recvMessageType(in);
			/* If this is not a valid ACK, it could be an error report*/
			if (REPLY_ACK_MSG_EC.compareTo(
					msgType.substring(
							0, REPLY_ACK_MSG_EC.length())) != 0) {
				recvError(msgType, in);
			}
		} catch (IOException e) {
			throw (new RequestException(e));
		}
		if (LOG.isDebugEnabled()) {
			LOG.debug("Connection:" + mConnectionID + " Ack received.");
		}
	}
	
	/**
	 * Received chunked data until requested number of bytes is received.
	 * @param in an opened input stream on the host
	 * @param size the number of bytes to receive
	 * @return byte array containing received data
	 * @throws RequestException if receive fails
	 */
	private byte[] receiveSize(
			final InputStream in,
			final int size) throws RequestException {
		byte[] buffer = new byte[size];
		int recvd = 0;
		try {
			while (recvd < size) {
				recvd += in.read(buffer, recvd, size - recvd);
			}
		} catch (IOException e) {
			throw (new RequestException(e));
		}
		return buffer;
	}
	
	/**
	 * Receives character data from host and convert it from  character set.
	 * It is assumed the maximum size to receive is small and is unlikely to
	 * get chunked.
	 * @param in an it stream from a socket connection to a host
	 * @param charset the host character set
	 * @param maxSize the largest expected size
	 * @return the result string
	 * @throws RequestException if receiving fails
	 */
	private static String getCharsFromHost(
			final InputStream  in,
			final String charset,
			final int maxSize) throws RequestException {
		String str = null;
		try {
			byte[] buffer = new byte[maxSize];
			int size = in.read(buffer);
			if (size == -1) {
				return null;
			}
			str = new String(buffer, 0, size, charset);
		} catch (UnsupportedEncodingException e) {
			throw (new RequestException(e));
		} catch (IOException e) {
			throw (new RequestException(e));
		}
		return str;
	}
	
	/**
	 * The Client Initial Message (CIM) is data that the IBM CICS Socket
	 * listener will pass to security exist and then to the server
	 * transaction. It is used by the LegStar protocol to pass data that
	 * remains valid for the entire duration of a connection.
	 * 
	 * @param cicsUserID the CICS User ID to authenticate/impersonate
	 * @param cicsPassword the CICS User credentials
	 * @param connectionID A unique string representing this connection
	 * @param trace enable trace mode
	 * @param charset the host character set
	 * @return the formatted CIM
	 * @throws CicsSocketHostConversionException if initial message formatting
	 *  fails
	 */
	public static byte[] formatCIM(
			final String cicsUserID,
			final String cicsPassword,
			final String connectionID,
			final boolean trace,
			final String charset) throws CicsSocketHostConversionException {
		byte[] cicsCIM = new byte[CIM_LEN];
		int pos = 0;
		toHostBytes(cicsUserID, cicsCIM, pos, CICS_USERID_LEN, charset);
		pos += CICS_USERID_LEN;
		toHostBytes(cicsPassword, cicsCIM, pos, CICS_PWD_LEN, charset);
		pos += CICS_PWD_LEN;
		toHostBytes(connectionID, cicsCIM, pos, CONNECTION_ID_LEN, charset);
		pos += CONNECTION_ID_LEN;
		if (trace) {
			toHostBytes("1", cicsCIM, pos, TRACE_LEN, charset);
		} else {
			toHostBytes("0", cicsCIM, pos, TRACE_LEN, charset);
		}
		pos += TRACE_LEN;
		toHostBytes(CIM_EYE_CATCHER, cicsCIM, pos, CIM_EYE_CATCHER.length(),
				charset);
		pos += CIM_EYE_CATCHER.length();
		return cicsCIM;
	}
	
	/**
	 * Formats the header of a message part, ready for serialization. 
	 * This includes the message part ID in host charset encoding as 
	 * well as the length of the content (4 bytes, big endian).
	 * 
	 * @param messageType message part type
	 * @param messagePart the message part to serialize
	 * @param charset the host character set
	 * @return the serialized header
	 * @throws CicsSocketHostConversionException if formatting fails
	 */
	public static byte[] formatMPH(
			final String messageType,
			final MessagePart messagePart,
			final String charset) throws CicsSocketHostConversionException {
		byte[] aMPHBytes = new byte[MPH_LEN];
		int pos = 0;
		toHostBytes(messageType, aMPHBytes, pos,
				MSG_TYPE_LEN, charset);
		pos += MSG_TYPE_LEN;
		toHostBytes(messagePart.getID(), aMPHBytes, pos,
				MSG_PART_ID_LEN, charset);
		pos += MSG_PART_ID_LEN;
		ByteBuffer bb = ByteBuffer.allocate(4);
		bb.putInt((messagePart.getContent() == null)
				? 0 : messagePart.getContent().length);
		bb.flip();
		bb.get(aMPHBytes, pos, 4);
		pos += 4;
		return aMPHBytes;
	}
	
	/**
	 * Formats the UOW command instructing host on how to deal with
	 * the current unit of work.
	 * 
	 * @param command the UOW processing command
	 * @param charset the host character set
	 * @return the serialized header
	 * @throws CicsSocketHostConversionException if formatting fails
	 */
	public static byte[] formatUOW(
			final String command,
			final String charset) throws CicsSocketHostConversionException {
		byte[] aUOWBytes = new byte[UOW_MSG_EC_LEN + 1 + UOW_COMMAND_LEN + 1];
		int pos = 0;
		toHostBytes(UOW_MSG_EC, aUOWBytes, pos,
				UOW_MSG_EC_LEN, charset);
		pos += (UOW_MSG_EC_LEN + 1);
		toHostBytes(command, aUOWBytes, pos,
				command.length(), charset);
		pos += (UOW_COMMAND_LEN + 1);
		return aUOWBytes;
	}
	
	/**
	 * Formats the probe request.
	 * 
	 * @param charset the host character set
	 * @return the serialized header
	 * @throws CicsSocketHostConversionException if formatting fails
	 */
	public static byte[] formatProbe(
			final String charset) throws CicsSocketHostConversionException {
		byte[] probeBytes = new byte[PROBE_REQUEST_EC.length() + 1];
		int pos = 0;
		toHostBytes(PROBE_REQUEST_EC, probeBytes, pos,
				PROBE_REQUEST_EC.length(), charset);
		pos += (PROBE_REQUEST_EC.length() + 1);
		return probeBytes;
	}
	
	/**
	 * Given a host charset, this method converts a string into host bytes and
	 * either truncates or fills with spaces so that the result fits in the
	 * requested hostLength.
	 * 
	 * @param clientString the string in unicode
	 * @param hostBytes the host buffer to fill
	 * @param startPos first byte to fill in hostBytes
	 * @param hostLength total bytes to fill in hostBytes
	 * @param hostCharset host character set
	 * @throws CicsSocketHostConversionException if conversion fails
	 */
	public static void toHostBytes(
			final String clientString,
			final byte[] hostBytes,
			final int startPos,
			final int hostLength,
			final String hostCharset) throws CicsSocketHostConversionException {
		byte[] clientBytes;
		byte[] spaceBytes;
		try {
			clientBytes = clientString.getBytes(hostCharset);
			spaceBytes = " ".getBytes(hostCharset);
		} catch (UnsupportedEncodingException e) {
			throw (new CicsSocketHostConversionException(e));
		}
		int j = 0;
		for (int i = startPos; i < startPos + hostLength; i++) {
			if (j < clientBytes.length) {
				hostBytes[i] = clientBytes[j];
				j++;
			} else {
				hostBytes[i] = spaceBytes[0];
			}
		}
	}

	/**
	 * @return the current socket connection
	 */
	public final Socket getClientSocket() {
		return mClientSocket;
	}

	/**
	 * @param clientSocket a socket connection to a CICS region
	 */
	public final void setClientSocket(final Socket clientSocket) {
		mClientSocket = clientSocket;
	}

	/**
	 * @return the CICS socket endpoint
	 */
	public final CicsSocketEndpoint getCicsSocketEndpoint() {
		return mCicsSocketEndpoint;
	}

	/**
	 * @param cicsSocketEndpoint the CICS socket endpoint to set
	 */
	public final void setCicsSocketEndpoint(
			final CicsSocketEndpoint cicsSocketEndpoint) {
		mCicsSocketEndpoint = cicsSocketEndpoint;
	}

	/**
	 * @return the identifier for this connection
	 */
	public final String getConnectionID() {
		return mConnectionID;
	}

	/**
	 * @param connectionID an identifier for this connection to set
	 */
	public final void setConnectionID(final String connectionID) {
		mConnectionID = connectionID;
	}

	/** (non-Javadoc).
	 * @see com.legstar.messaging.Connection#setConnectTimeout(int)
	 * {@inheritDoc}
	 */
	public final void setConnectTimeout(final long timeout) {
		mConnectTimeout = new Long(timeout).intValue();
	}

	/** (non-Javadoc).
	 * @see com.legstar.messaging.Connection#setReceiveTimeout(int)
	 * {@inheritDoc}
	 */
	public final void setReceiveTimeout(final long timeout) {
		mReceiveTimeout = new Long(timeout).intValue();
	}

	/** (non-Javadoc).
	 * @see com.legstar.messaging.Connection#getConnectTimeout()
	 * {@inheritDoc}
	 */
	public final long getConnectTimeout() {
		return mConnectTimeout;
	}

	/** (non-Javadoc).
	 * @see com.legstar.messaging.Connection#getReceiveTimeout()
	 * {@inheritDoc}
	 */
	public final long getReceiveTimeout() {
		return mReceiveTimeout;
	}
	

}


package com.legstar.test.cixs.lsfileae;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;

/**
 * Host connectivity related elements that clients can act upon.
 * 
 * This class was generated by CIXS version 1.0.
 * 2007-02-06T14:30:50.828+01:00
 */

@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LsfileaeHostHeader",
         namespace = "http://cixs.test.legstar.com/lsfileae",
         propOrder = {
    "hostUser", 
	"hostPassword", 
	"hostIPAddress", 
	"hostIPPort", 
	"hostCICWPath" 
})
public class LsfileaeHostHeader {
	
    /** User ID used for host authentication/impersonation. */
    @XmlElement(name = "hostUser",
                namespace = "http://cixs.test.legstar.com/lsfileae",
                required = true)
    private String hostUser;

    /** Password used for authentication. */
    @XmlElement(name = "hostPassword",
                namespace = "http://cixs.test.legstar.com/lsfileae",
                required = true)
    private String hostPassword;

    /** TCPIP address of the Host. */
    @XmlElement(name = "hostIPAddress",
                namespace = "http://cixs.test.legstar.com/lsfileae",
                required = true)
    private String hostIPAddress;

    /** TCPIP port number on which the Host listens. */
    @XmlElement(name = "hostIPPort",
                namespace = "http://cixs.test.legstar.com/lsfileae",
                required = true)
    private int hostIPPort = 0;

    /** The Path to the HTTP server on the Host. */
    @XmlElement(name = "hostCICWPath",
                namespace = "http://cixs.test.legstar.com/lsfileae",
                required = true)
    private String hostCICWPath;

	/**
	 * Gets the Path to the HTTP server on the Host.
	 * @return the Path to the HTTP server
	 */
	public final String getHostCICWPath() {
		return hostCICWPath;
	}

	/**
	 * Sets the path to the HTTP server on the Host.
	 * @param path the path to set
	 */
	public final void setHostCICWPath(final String path) {
		this.hostCICWPath = path;
	}

	/**
	 * Gets the TCPIP address of the Host.
	 * @return the TCPIP address
	 */
	public final String getHostIPAddress() {
		return hostIPAddress;
	}

	/**
	 * Sets the TCPIP address of the Host.
	 * @param address the TCPIP address to set
	 */
	public final void setHostIPAddress(final String address) {
		this.hostIPAddress = address;
	}

	/**
	 * Gets the TCPIP port number on which the Host listens.
	 * @return the TCPIP address
	 */
	public final int getHostIPPort() {
		return hostIPPort;
	}

	/**
	 * Sets the TCPIP port number on which the Host listens.
	 * @param port the TCPIP port to set
	 */
	public final void setHostIPPort(final int port) {
		this.hostIPPort = port;
	}

	/** Gets the user ID used for host authentication/impersonation.
	 * @return host user ID
	 */
	public final String getHostUser() {
		return hostUser;
	}

	/**
	 * Sets the user ID used for host authentication/impersonation.
	 * @param user host user ID to set
	 */
	public final void setHostUser(final String user) {
		this.hostUser = user;
	}

	/** 
	 * Gets the password used for authentication.
	 * @return host user ID
	 */
	public final String getHostPassword() {
		return hostPassword;
	}

	/**
	 * Sets the password used for authentication.
	 * @param password host user ID to set
	 */
	public final void setHostPassword(final String password) {
		this.hostPassword = password;
	}

}


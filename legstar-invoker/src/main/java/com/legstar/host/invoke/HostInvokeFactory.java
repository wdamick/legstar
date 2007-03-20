package com.legstar.host.invoke;

import com.legstar.messaging.Address;

/**
 * A factory providing a host invoke capability. In this version, only
 * one such capability is provided.
 */
public final class HostInvokeFactory {
	
	/**
	 * This factory is a utility class.
	 */
	private HostInvokeFactory() {
		
	}

	/**
	 * An Invoker is constructed from a configuration file, for a particular
	 * host address and target host program.
	 * @param generalConfigFileName an XML configuration file name
	 * @param address the host address
	 * @param programAttributesFileName the host program attributes properties
	 * file
	 * @return a Host invoke implementation
	 * @throws HostInvokeException in construction fails
	 */
	public static HostInvoke createHostInvoke(
			final String generalConfigFileName,
			final Address address,
			final String programAttributesFileName)
			throws HostInvokeException {
		return new CommareaInvoke(
				generalConfigFileName, address, programAttributesFileName);
	}

}

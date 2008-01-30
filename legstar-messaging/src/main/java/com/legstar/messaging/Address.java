package com.legstar.messaging;

import org.apache.commons.configuration.HierarchicalConfiguration;

/** @deprecated */
public class Address extends LegStarAddress {

	/**
	 * @param endPointName the target endpoint name
	 */
	public Address(final String endPointName) {
		super(endPointName);
	}

	/**
	 * @param address the partial address
	 * @param endpointConfig an XML configuration fragment
	 */
	public Address(final LegStarAddress address,
			final HierarchicalConfiguration endpointConfig) {
		super(address, endpointConfig);
	}

	/**
	 * @param endpointConfig an XML configuration fragment
	 */
	public Address(final HierarchicalConfiguration endpointConfig) {
		super(endpointConfig);
	}

}

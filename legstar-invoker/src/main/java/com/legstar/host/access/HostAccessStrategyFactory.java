package com.legstar.host.access;

import org.apache.commons.configuration.HierarchicalConfiguration;

/**
 * A factory providing a host access strategy. The host access strategy is
 * configurable.
 */
public final class HostAccessStrategyFactory {
	
	/** Relative XPath location within enpoint for host access strategy. */
	private static final String HOST_ACCESS_STRATEGY_CFG =
		"hostAccessStrategy";

	/** Configuration value for direct host access. */
	private static final String DIRECT_HOST_ACCESS_STRATEGY = "direct";
	
	/** Configuration value for pooled host access. */
	private static final String POOLED_HOST_ACCESS_STRATEGY = "pooled";
	
	/** If not configured, we will use a direct strategy. */
	private static final String DEFAULT_HOST_ACCESS_STRATEGY =
		"direct";
	
	/**
	 * This factory is a utility class.
	 */
	private HostAccessStrategyFactory() {
		
	}
	/**
	 * Delivers a host access strategy.
	 * @param endpointConfig an XML configuration sub-hierarchy for an endpoint
	 * @return the new host access strategy
	 * @throws HostAccessStrategyException if creation failed
	 */
	public static HostAccessStrategy createAccessStrategy(
			final HierarchicalConfiguration endpointConfig)
			throws HostAccessStrategyException {
		String hostAccessStrategy = endpointConfig.getString(
				HOST_ACCESS_STRATEGY_CFG, DEFAULT_HOST_ACCESS_STRATEGY);
		if (hostAccessStrategy.trim().compareToIgnoreCase(
				DIRECT_HOST_ACCESS_STRATEGY) == 0) {
			return new DirectHostAccessStrategy(endpointConfig);
		} else {
			if (hostAccessStrategy.trim().compareToIgnoreCase(
					POOLED_HOST_ACCESS_STRATEGY) == 0) {
				return new PooledHostAccessStrategy(endpointConfig);
			} else {
				throw new HostAccessStrategyException(
						"Unknown host access strategy.");
			}
		}
		
	}

}

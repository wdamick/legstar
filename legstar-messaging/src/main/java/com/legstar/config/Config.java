package com.legstar.config;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.List;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.commons.configuration.tree.xpath.XPathExpressionEngine;

import com.legstar.messaging.Address;
import com.legstar.messaging.ConnectionFactory;

/**
 * This utility class provides general methods to manipulate the LegStar
 * XML configuration based on Apache commons Configuration.
 */
public final class Config {

	/** Config is a utility class. */
	private Config() {
		
	}
	
    /**
     * Loads an XML configuration from file.
     * @param configFileName the configuration file name
     * @return the in-memory XML configuration
     * @throws ConfigurationException if configuration failed to load
     */
    public static HierarchicalConfiguration loadGeneralConfig(
            final String configFileName) throws  ConfigurationException {
        XMLConfiguration generalConfig = null;
        generalConfig = new XMLConfiguration(configFileName);
        generalConfig.setExpressionEngine(new XPathExpressionEngine());
        return generalConfig;
    }
    
	/**
	 * Get the configuration sub-hierarchy for the endpoint
	 * specified in the client request.
	 * @param generalConfig the general configuration
	 * @param endpointName the requested endpoint
	 * @return the configuration sub hierarchy
	 * @throws ConfigurationException if failed to load configuration
	 */
	public static HierarchicalConfiguration loadEndpointConfiguration(
			final HierarchicalConfiguration generalConfig,
			final String endpointName) throws ConfigurationException {
		
		String strXPath = Constants.HOST_ENDPOINT_KEY
		+ "[@name='" + endpointName + "']";
		List  endpoints = generalConfig.configurationsAt(strXPath);
		if (endpoints == null || endpoints.isEmpty()) {
			throw new ConfigurationException("The requested endpoint:" 
					+ endpointName
					+ " is not defined.");
		}
		return (HierarchicalConfiguration) endpoints.get(0);
	}
	
	/**
	 * Get the first endpoint definition as the default one.
	 * @param generalConfig the general configuration
	 * @return the configuration sub hierarchy
	 * @throws ConfigurationException if failed to load configuration
	 */
	public static HierarchicalConfiguration loadDefaultEndpointConfiguration(
			final HierarchicalConfiguration generalConfig)
			throws ConfigurationException {
		
		String strXPath = Constants.HOST_ENDPOINT_KEY;
		List  endpoints = generalConfig.configurationsAt(strXPath);
		if (endpoints == null || endpoints.isEmpty()) {
			throw new ConfigurationException(
					"There are no endpoints defined.");
		}
		return (HierarchicalConfiguration) endpoints.get(0);
	}
	
	/**
	 * Find endpoint configuration for a requested address. If not
	 * found, return the default endpoint configuration.
	 * @param generalConfig the general configuration
	 * @param address the requested host address
	 * @return the configuration sub hierarchy
	 * @throws ConfigurationException if failed to load configuration
	 */
	public static HierarchicalConfiguration loadAddressConfiguration(
			final HierarchicalConfiguration generalConfig,
			final Address address) throws ConfigurationException {
		HierarchicalConfiguration endpointConfig = null;
		if (address == null || address.getEndPointName() == null
				|| address.getEndPointName().length() == 0) {
			endpointConfig = Config.loadDefaultEndpointConfiguration(
					generalConfig);
		} else {
			endpointConfig = Config.loadEndpointConfiguration(
					generalConfig,
					address.getEndPointName());
		}
		return endpointConfig;
	}

	/**
	 * The connection factory shields the client from transport. To
	 * achieve this we use the abstract factory pattern to load a
	 * connection factory dynamically.
	 * 
	 * @param endpointConfig the endpoint configuration
	 * @return a new connection factory
	 * @throws ConfigurationException if connection factory cannot be
	 *  created
	 */
	public static ConnectionFactory loadConnectionFactory(
			final HierarchicalConfiguration endpointConfig)
			throws ConfigurationException {
		
		/* Get the name of the connection factory from the configuration	*/
		String factoryClass =
			endpointConfig.getString(Constants.HOST_CONNECTION_FACTORY_KEY);
		if (factoryClass == null || factoryClass.length() == 0) {
			throw new ConfigurationException(
				"There are no connection factories in the configuration.");
		}
		
		/* Instantiate the factory */
		ConnectionFactory cFactory = null;
		try {
			Class factoryClazz = Class.forName(factoryClass);
			Constructor constructor = factoryClazz.getConstructor(
					new Class[] {HierarchicalConfiguration.class});
			cFactory = (ConnectionFactory) constructor.newInstance(
					new Object[] {endpointConfig});
		} catch (ClassNotFoundException e) {
			throw new ConfigurationException(e);
		} catch (InstantiationException e) {
			throw new ConfigurationException(e);
		} catch (IllegalAccessException e) {
			throw new ConfigurationException(e);
		} catch (SecurityException e) {
			throw new ConfigurationException(e);
		} catch (NoSuchMethodException e) {
			throw new ConfigurationException(e);
		} catch (IllegalArgumentException e) {
			throw new ConfigurationException(e);
		} catch (InvocationTargetException e) {
			throw new ConfigurationException(e);
		}
		return cFactory;
	}


}

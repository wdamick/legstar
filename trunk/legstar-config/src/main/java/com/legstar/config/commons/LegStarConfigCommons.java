package com.legstar.config.commons;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.DefaultConfigurationBuilder;
import org.apache.commons.configuration.HierarchicalConfiguration;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.commons.configuration.tree.xpath.XPathExpressionEngine;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.config.LegStarConfigurationException;
import com.legstar.coxb.util.Utils;
import com.legstar.config.PoolingEngineConfig;
import com.legstar.messaging.ConnectionFactory;
import com.legstar.messaging.HostEndpoint;
import com.legstar.messaging.LegStarAddress;
import com.legstar.messaging.HostEndpoint.AccessStrategy;

/**
 * A configuration system based on apache commons configuration.
 * 
 */
public class LegStarConfigCommons {

    /** XML Configuration. */
    private HierarchicalConfiguration _generalConfig;

    /** Configuration XPath location for an endpoint. */
    public static final String HOST_ENDPOINT_KEY =
            "hostEndPoints/hostEndPoint";

    /** Configuration key giving the work manager JNDI location. */
    private static final String WORK_MANAGER_LOCATION_KEY =
            "engine/workManager/threadPool/JNDILocation";

    /** Configuration key giving the work manager thread pool size. */
    private static final String WORK_MANAGER_THREAD_POOL_SIZE_KEY =
            "engine/workManager/defaultThreadPool/size";

    /**
     * Configuration key giving maximum number of requests waiting to
     * be serviced.
     */
    private static final String POOLING_MAXIMUM_REQUESTS_KEY =
            "engine/maxRequests";

    /**
     * the maximum time (milliseconds) to wait for a pooled connection to become
     * available.
     */
    private static final String TAKE_TIMEOUT_KEY =
            "engine/takeTimeout";

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * Construct from a configuration file.
     * 
     * @param generalConfigFileName the configuration file name
     * @throws LegStarConfigurationException if configuration file cannot be
     *             found or is invalid
     */
    public LegStarConfigCommons(
            final String generalConfigFileName)
            throws LegStarConfigurationException {
        _generalConfig = loadGeneralConfig(generalConfigFileName);
    }

    /**
     * Lookup and endpoint by name.
     * 
     * @param endpointName the endpoint name and identifier
     *            matches the requested endpoint name
     * @return a host endpoint
     * @throws LegStarConfigurationException if endpoint not found
     */
    public HostEndpoint getHostEndpoint(
            final String endpointName) throws LegStarConfigurationException {
        if (_log.isDebugEnabled()) {
            _log.debug("Looking up endpoint: " + endpointName);
        }
        String strXPath = HOST_ENDPOINT_KEY + "[@name='" + endpointName + "']";
        List < ? > endpoints = _generalConfig.configurationsAt(strXPath);
        if (endpoints == null || endpoints.isEmpty()) {
            throw new LegStarConfigurationException("The requested endpoint:"
                    + endpointName
                    + " is not defined.");
        }
        return getHostEndpoint((HierarchicalConfiguration) endpoints.get(0));
    }

    /**
     * lookup an endpoint corresponding to a given address. If the address
     * is empty or does not specify an endpoint name, we return a default.
     * 
     * @param address the address to match with an endpoint
     * @return the matching endpoint or the default one
     * @throws LegStarConfigurationException if no endpoint can be returned
     */
    public HostEndpoint getHostEndpoint(
            final LegStarAddress address) throws LegStarConfigurationException {
        if (_log.isDebugEnabled()) {
            _log
                    .debug("Searching for an endpoint matching address: "
                            + address);
        }
        if (address == null || address.getEndPointName() == null
                || address.getEndPointName().length() == 0) {
            return getDefaultHostEndpoint();
        } else {
            return getHostEndpoint(address.getEndPointName());
        }
    }

    /**
     * Lookup the default endpoint configuration.
     * 
     * @return the default host endpoint
     * @throws LegStarConfigurationException if no endpoints found
     */
    public HostEndpoint getDefaultHostEndpoint()
            throws LegStarConfigurationException {
        if (_log.isDebugEnabled()) {
            _log.debug("Searching for default endpoint");
        }
        String strXPath = HOST_ENDPOINT_KEY;
        List < ? > endpoints = _generalConfig.configurationsAt(strXPath);
        if (endpoints == null || endpoints.isEmpty()) {
            throw new LegStarConfigurationException(
                    "There are no endpoints defined.");
        }
        return getHostEndpoint((HierarchicalConfiguration) endpoints.get(0));
    }

    /**
     * @return the list of all available host endpoints.
     * @throws LegStarConfigurationException if configuration is invalid
     */
    @SuppressWarnings("unchecked")
    public List < HostEndpoint > getHostEndpoints()
            throws LegStarConfigurationException {
        if (_log.isDebugEnabled()) {
            _log.debug("Searching for all endpoints");
        }
        List < HostEndpoint > endpoints = new ArrayList < HostEndpoint >();
        String strXPath = HOST_ENDPOINT_KEY;
        List < HierarchicalConfiguration > endpointConfigs =
                (List < HierarchicalConfiguration >) _generalConfig
                        .configurationsAt(strXPath);
        for (HierarchicalConfiguration endpointConfig : endpointConfigs) {
            endpoints.add(getHostEndpoint(endpointConfig));
        }

        return endpoints;
    }

    /**
     * @return the pooling engine configuration
     * @throws LegStarConfigurationException if cannot be created
     */
    public PoolingEngineConfig getPoolingEngineConfig()
            throws LegStarConfigurationException {
        if (_log.isDebugEnabled()) {
            _log.debug("Searching for pooling engine configuration");
        }
        PoolingEngineConfig poolingEngineConfig = new PoolingEngineConfig();
        poolingEngineConfig.setMaxRequests(
                _generalConfig.getInt(POOLING_MAXIMUM_REQUESTS_KEY,
                        PoolingEngineConfig.DEFAULT_MAXIMUM_REQUESTS));
        poolingEngineConfig.setThreadPoolSize(
                _generalConfig.getInt(WORK_MANAGER_THREAD_POOL_SIZE_KEY,
                        PoolingEngineConfig.DEFAULT_THREAD_POOL_SIZE));
        poolingEngineConfig.setWorkManagerJNDILocation(
                _generalConfig.getString(WORK_MANAGER_LOCATION_KEY));
        poolingEngineConfig.setHostEndpoints(getHostEndpoints());
        poolingEngineConfig.setTakeTimeout(
                _generalConfig.getInt(TAKE_TIMEOUT_KEY,
                        PoolingEngineConfig.DEFAULT_TAKE_TIMEOUT));
        return poolingEngineConfig;
    }

    /**
     * Loads an XML configuration from file.
     * 
     * @param configFileName the configuration file name
     * @return the in-memory XML configuration
     * @throws LegStarConfigurationException if configuration failed to load
     */
    protected HierarchicalConfiguration loadGeneralConfig(
            final String configFileName) throws LegStarConfigurationException {
        try {
            if (_log.isDebugEnabled()) {
                _log.debug("Loading configuration file: " + configFileName);
            }
            /* First try as if it is a single configuration file */
            HierarchicalConfiguration generalConfig = new XMLConfiguration(
                    configFileName);
            /*
             * If the first tag is additional, then this is a combined
             * configuration
             * that needs to be loaded in a specific way.
             */
            if (generalConfig.configurationsAt("additional").size() > 0) {
                DefaultConfigurationBuilder dcb = new DefaultConfigurationBuilder();
                dcb.setFileName(configFileName);
                generalConfig = (HierarchicalConfiguration) dcb
                        .getConfiguration(true).getConfiguration(
                                DefaultConfigurationBuilder.ADDITIONAL_NAME);
            }
            generalConfig.setExpressionEngine(new XPathExpressionEngine());
            return generalConfig;
        } catch (ConfigurationException e) {
            throw new LegStarConfigurationException(e);
        }
    }

    /**
     * Turn a commons configuration tree into a bean.
     * 
     * @param endpointConfig the commons configuration subtree
     * @return a host endpoint bean
     * @throws LegStarConfigurationException if bean cannot be created
     */
    protected HostEndpoint getHostEndpoint(
            final HierarchicalConfiguration endpointConfig)
            throws LegStarConfigurationException {
        ConnectionFactory connectionFactory = loadConnectionFactory(endpointConfig);
        HostEndpoint endpoint = connectionFactory.createEndpoint();
        setValues(endpoint, endpointConfig);
        return endpoint;
    }

    /**
     * The connection factory shields the client from transport. To
     * achieve this we use the abstract factory pattern to load a
     * connection factory dynamically.
     * 
     * @param endpointConfig the endpoint configuration
     * @return a new connection factory
     * @throws LegStarConfigurationException if connection factory cannot be
     *             created
     */
    protected ConnectionFactory loadConnectionFactory(
            final HierarchicalConfiguration endpointConfig)
            throws LegStarConfigurationException {

        /* Get the name of the connection factory from the configuration */
        String factoryClass =
                endpointConfig
                        .getString(HostEndpoint.HOST_CONNECTION_FACTORY_CLASS_LABEL);
        if (factoryClass == null || factoryClass.length() == 0) {
            throw new LegStarConfigurationException(
                    "There are no connection factories in the configuration.");
        }

        if (_log.isDebugEnabled()) {
            _log.debug("Loading connection factory class: " + factoryClass);
        }
        try {
            Class < ? > factoryClazz = Utils.loadClass(factoryClass);
            return (ConnectionFactory) factoryClazz.newInstance();
        } catch (ClassNotFoundException e) {
            throw new LegStarConfigurationException(e);
        } catch (InstantiationException e) {
            throw new LegStarConfigurationException(e);
        } catch (IllegalAccessException e) {
            throw new LegStarConfigurationException(e);
        } catch (SecurityException e) {
            throw new LegStarConfigurationException(e);
        } catch (IllegalArgumentException e) {
            throw new LegStarConfigurationException(e);
        }
    }

    /**
     * Sets bean members using configuration attributes.
     * 
     * @param endpoint the bean
     * @param endpointConfig the configuration hierarchy
     * @throws LegStarConfigurationException if setting value on bean fails
     */
    protected void setValues(
            final Object endpoint,
            final HierarchicalConfiguration endpointConfig)
            throws LegStarConfigurationException {
        for (Iterator < ? > iterator = endpointConfig.getKeys(); iterator
                .hasNext();) {
            String key = (String) iterator.next();
            setValue(endpoint, key, endpointConfig.getString(key));
        }
    }

    /**
     * For a given configuration key and value locate a corresponding
     * setter method and use it.
     * 
     * @param o the bean to set
     * @param key the configuration key
     * @param value the configuration value
     * @throws LegStarConfigurationException if setting value on bean fails
     */
    protected void setValue(
            final Object o,
            final String key,
            final String value) throws LegStarConfigurationException {
        String setterName = getSetterName(key);
        if (_log.isDebugEnabled()) {
            _log.debug("Using setter method: " + setterName + ", for value: "
                    + value);
        }
        try {
            Method[] allMethods = o.getClass().getMethods();
            for (Method method : allMethods) {
                if (method.getName().equals(setterName)) {
                    method.setAccessible(true);
                    Class < ? > parm = (Class < ? >) method
                            .getGenericParameterTypes()[0];
                    if (parm.isAssignableFrom(String.class)) {
                        method.invoke(o, value);
                        break;
                    }
                    if (parm.isAssignableFrom(boolean.class)) {
                        method.invoke(o, Boolean.parseBoolean(value));
                        break;
                    }
                    if (parm.isAssignableFrom(int.class)) {
                        method.invoke(o, Integer.parseInt(value));
                        break;
                    }
                    if (parm.isAssignableFrom(long.class)) {
                        method.invoke(o, Long.parseLong(value));
                        break;
                    }
                    if (parm
                            .isAssignableFrom(HostEndpoint.AccessStrategy.class)) {
                        method.invoke(o, AccessStrategy.valueOf(value));
                        break;
                    }
                    _log.warn("Setter method: " + setterName
                            + ", parameter type: "
                            + parm + ", not compatible with value: " + value);
                }
            }
        } catch (SecurityException e) {
            throw new LegStarConfigurationException(e);
        } catch (IllegalArgumentException e) {
            throw new LegStarConfigurationException(e);
        } catch (IllegalAccessException e) {
            throw new LegStarConfigurationException(e);
        } catch (InvocationTargetException e) {
            throw new LegStarConfigurationException(e);
        }
    }

    /**
     * Return a setter method name corresponding of a configuration parameter.
     * 
     * @param key the configuration parameter key
     * @return a bean setter method name
     */
    protected static String getSetterName(final String key) {
        String setterName = key;
        if (setterName == null || setterName.length() == 0) {
            return null;
        }
        if (setterName.charAt(0) == '@') {
            setterName = setterName.substring(1);
        }
        String prefix = "set"
                + setterName.substring(0, 1).toUpperCase(Locale.getDefault());
        return prefix + setterName.substring(1);

    }
}

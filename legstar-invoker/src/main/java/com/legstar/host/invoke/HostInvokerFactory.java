/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.host.invoke;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalConfiguration;

import com.legstar.config.Config;
import com.legstar.host.access.HostAccessStrategy;
import com.legstar.host.access.HostAccessStrategyException;
import com.legstar.host.access.HostAccessStrategyFactory;
import com.legstar.messaging.LegStarAddress;

/**
 * A factory providing a host invoke capability. Based on the target host
 * program attributes, the factory selects an appropriate invoker.
 */
public final class HostInvokerFactory {

    /** XML Configuration as a class field, in order to load only once. */
    private static HierarchicalConfiguration sGeneralConfig;

    /**
     * This factory is a utility class.
     */
    private HostInvokerFactory() {

    }

    /**
     * An Invoker is constructed from a configuration file, for a particular
     * host address and target host program.
     * @param generalConfigFileName an XML configuration file name
     * @param address the host address
     * @param cicsProgramFileName the host program attributes properties
     * file
     * @return a Host invoke implementation
     * @throws HostInvokerException in construction fails
     */
    public static HostInvoker createHostInvoker(
            final String generalConfigFileName,
            final LegStarAddress address,
            final String cicsProgramFileName)
    throws HostInvokerException {

        /* Load the XML configuration, if necessary */
        if (sGeneralConfig == null) {
            sGeneralConfig = loadGeneralConfig(generalConfigFileName);
        }

        /* load the program properties file */
        CicsProgram hostProgram =
            new CicsProgram(cicsProgramFileName);

        /* Load endpoint configuration specified in requested address or the
         * default one if address is empty.  */
        HierarchicalConfiguration endpointConfig = null;
        try {
            endpointConfig = Config.loadAddressConfiguration(
                    sGeneralConfig, address);
        } catch (ConfigurationException e) {
            throw new HostInvokerException(e);
        }

        /* Create a complete address by merging requested address attributes
         * with the endpoint configuration ones. For instance if requested
         * address does not specify credentials, we will pick up the default
         * credentials from the configuration. */
        LegStarAddress completeAddress =
            new LegStarAddress(address, endpointConfig);

        /* Load a host access strategy */
        HostAccessStrategy hostAccessStrategy;
        try {
            hostAccessStrategy =
                HostAccessStrategyFactory.createAccessStrategy(endpointConfig);
        } catch (HostAccessStrategyException e) {
            throw new HostInvokerException(e);
        }

        /* If a Channel is specified, this is a request for LINK CHANNEL */
        String channel = hostProgram.getChannel();
        if (channel != null && channel.length() > 0) {
            return new ContainerInvoker(
                    hostAccessStrategy, completeAddress, hostProgram);
        }

        /* The default is a commarea invoke */
        return new CommareaInvoker(
                hostAccessStrategy, completeAddress, hostProgram);
    }

    /**
     * Backward compatibility.
     * An Invoker is constructed from a configuration file, for a particular
     * host address and target host program.
     * @param generalConfigFileName an XML configuration file name
     * @param address the host address
     * @param cicsProgramFileName the host program attributes properties
     * file
     * @return a Host invoke implementation
     * @throws HostInvokerException in construction fails
     */
    @SuppressWarnings("deprecation")
    public static HostInvoker createHostInvoker(
            final String generalConfigFileName,
            final com.legstar.messaging.Address address,
            final String cicsProgramFileName)
    throws HostInvokerException {
        return createHostInvoker(generalConfigFileName,
                (LegStarAddress) address, cicsProgramFileName);
    }
    /**
     * Loads an XML configuration from file.
     * @param configFileName the configuration file name
     * @return the in-memory XML configuration
     * @throws HostInvokerException if configuration failed to load
     */
    private static synchronized HierarchicalConfiguration loadGeneralConfig(
            final String configFileName) throws  HostInvokerException {
        /* Because the original test for nullity is not guarded, we might
         * still get into this synchronized code while some other thread
         * already assigned the static variable. */
        if (sGeneralConfig != null) {
            return sGeneralConfig;
        }
        try {
            HierarchicalConfiguration generalConfig =
                Config.loadGeneralConfig(configFileName);
            return generalConfig;
        } catch (ConfigurationException e) {
            throw new HostInvokerException(e);
        }
    }

}

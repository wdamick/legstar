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

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.List;
import java.util.Properties;

import com.legstar.config.Constants;
import com.legstar.host.invoke.model.HostContainer;
import com.legstar.host.invoke.model.HostProgram;
import com.legstar.host.invoke.model.HostProgramException;


/** 
 * This data object holds the characteristics of a target host program.
 * Properties are initially loaded from a Properties file.
 */
public class HostProgramProperties extends HostProgram {

    /**
     * Constructor from a properties file.
     * @param fileName the name of the property file
     * @throws HostProgramException if property file cannot be read
     */
    public HostProgramProperties(
            final String fileName) throws HostProgramException {

        /* load the program properties file */
        try {
            Properties props = load(fileName);
            setName(props.getProperty(Constants.CICS_PROGRAM_NAME_KEY));
            String strLength = props.getProperty(Constants.CICS_LENGTH_KEY, "0");
            setLength(Integer.parseInt(strLength));
            String strDataLength = props.getProperty(Constants.CICS_DATALEN_KEY, "0");
            setDataLength(Integer.parseInt(strDataLength));
            setSysID(props.getProperty(Constants.CICS_SYSID_KEY));
            String strSyncOnReturn = props.getProperty(Constants.CICS_SYNCONRET_KEY);
            if (strSyncOnReturn != null && strSyncOnReturn.length() > 0) {
                setSyncOnReturn(Boolean.parseBoolean(strSyncOnReturn));
            }
            setTransID(props.getProperty(Constants.CICS_TRANSID_KEY));
            /* When channel is used, there is a list of input containers and a
             * list of output containers. Each container has a name and a max size
             * (host bytes).
             *  */
            setChannel(props.getProperty(Constants.CICS_CHANNEL_KEY));
            if (getChannel() != null && getChannel().length() > 0) {
                loadContainer(props, getInContainers(),
                        Constants.CICS_IN_CONTAINERS_KEY,
                        Constants.CICS_IN_CONTAINERS_LEN_KEY);
                loadContainer(props, getOutContainers(),
                        Constants.CICS_OUT_CONTAINERS_KEY,
                        Constants.CICS_OUT_CONTAINERS_LEN_KEY);
            }
            check();
        } catch (IOException e) {
            throw new HostProgramException(e);
        } catch (NumberFormatException e) {
            throw new HostProgramException("Invalid length", e);
        }


    }

    /**
     * Create a map with container names and associated max size from entries
     * in a property file.
     * List of items are expected to be stored as a set of properties suffixed
     * with _n where n in the item rank. 
     * @param props program attributes as properties 
     * @param containers an empty map for containers names and sizes
     * @param nameKey the properties key for container name
     * @param lengthKey the properties key for container size
     * @throws HostProgramException if failed to create map
     */
    private void loadContainer(
            final Properties props,
            final List < HostContainer > containers,
            final String nameKey,
            final String lengthKey)
    throws HostProgramException {
        int i = 1;
        String name = props.getProperty(nameKey + '_' + i);
        while (name != null && name.length() > 0) {
            Integer length = Integer.valueOf(0);
            String slength = props.getProperty(
                    lengthKey + '_' + i);
            if (slength != null && slength.length() > 0) {
                length = Integer.parseInt(slength);
            }
            HostContainer container = new HostContainer();
            container.setName(name);
            container.setLength(length);
            containers.add(container);
            name = props.getProperty(nameKey + '_' + ++i);
        }
    }

    /**
     * Loads a properties file from classpath into memory.
     * @param propFileName the properties file name
     * @return the in-memory properties file
     * @throws IOException if file cannot be loaded
     */
    public Properties load(
            final String propFileName) throws IOException {

        /* load the program properties files */
        Properties properties = new Properties();

        /* We use the thread loader rather than the class loader because this
         * class will live in a different jar file than the calling
         * application. */
        ClassLoader classLoader = (ClassLoader) AccessController
        .doPrivileged(new PrivilegedAction < ClassLoader >() {
            public ClassLoader run() {
                return Thread.currentThread().getContextClassLoader();
            }
        });
        InputStream in = classLoader.getResourceAsStream(propFileName);
        if (in == null) {
            throw (new FileNotFoundException(propFileName));
        }
        properties.load(in);
        in.close();
        return properties;
    }

}

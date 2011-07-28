/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.clients.tests;

import java.io.File;
import java.util.Collection;

import junit.framework.TestCase;

import org.apache.commons.io.FileUtils;
import org.codehaus.cargo.container.InstalledLocalContainer;
import org.codehaus.cargo.container.configuration.entry.Resource;
import org.codehaus.cargo.container.deployable.WAR;
import org.codehaus.cargo.container.tomcat.Tomcat6xInstalledLocalContainer;
import org.codehaus.cargo.container.tomcat.Tomcat6xStandaloneLocalConfiguration;

/**
 * Common features for integration testing.
 * 
 */
public abstract class AbstractITCase extends TestCase {

    /** A Tomcat instance. */
    private InstalledLocalContainer _webapp;

    /**
     * Setup Tomcat.
     */
    public AbstractITCase() {
        _webapp = getContainer();
    }

    /**
     * Start a Tomcat instance.
     * */
    public void setUp() throws Exception {
        _webapp.start();
    }

    /**
     * Stop the Tomcat instance.
     * */
    public void tearDown() throws Exception {
        _webapp.stop();
    }

    /**
     * Create a Cargo web container deploying all war files. All dependencies go
     * to shared library. There is also an WMQ JNDI setup.
     * 
     * @return a container ready to start
     */
    @SuppressWarnings("unchecked")
    public InstalledLocalContainer getContainer() {

        Tomcat6xStandaloneLocalConfiguration configuration = new Tomcat6xStandaloneLocalConfiguration(
                "target/tomcat6x");
        Collection < File > warFiles = FileUtils
                .listFiles(new File("target/dependency/cixsgen/war"),
                        new String[] { "war" }, false);
        for (File warFile : warFiles) {
            configuration.addDeployable(new WAR(warFile.getAbsolutePath()));
        }
        Resource dsConnectionFactory = new Resource("ConnectionFactory",
                "com.ibm.mq.jms.MQQueueConnectionFactory");
        dsConnectionFactory.getParameters().put("hostName", "mainframe");
        dsConnectionFactory.getParameters().put("port", "1414");
        dsConnectionFactory.getParameters().put("queueManager", "CSQ1");
        dsConnectionFactory.getParameters().put("channel", "CLIENT.TO.CSQ1");
        dsConnectionFactory.getParameters().put("transportType", "1");
        configuration.addResource(dsConnectionFactory);

        configuration.addResource(getQueueResource("CicsARequestQueue",
                "CICSA.REQUEST.QUEUE"));
        configuration.addResource(getQueueResource("CicsAReplyQueue",
                "CICSA.REPLY.QUEUE"));
        configuration.addResource(getQueueResource("Cics01BridgeRequestQueue",
                "CICS01.BRIDGE.REQUEST.QUEUE"));
        configuration.addResource(getQueueResource("Cics01BridgeReplyQueue",
                "CICS01.BRIDGE.REPLY.QUEUE"));

        Tomcat6xInstalledLocalContainer webapp = new Tomcat6xInstalledLocalContainer(
                configuration);
        Collection < File > jarFiles = FileUtils
                .listFiles(new File("target/dependency/cixsgen/lib"),
                        new String[] { "jar" }, false);
        for (File jarFile : jarFiles) {
            webapp.addSharedClasspath(jarFile.getAbsolutePath());
        }
        webapp.addSharedClasspath(new File("target/dependency/cixsgen/conf")
                .getAbsolutePath());

        File wmqDir = new File(System.getenv("WMQ_HOME") + "/lib");
        webapp.addSharedClasspath(new File(wmqDir, "com.ibm.mq.jar")
                .getAbsolutePath());
        webapp.addSharedClasspath(new File(wmqDir, "com.ibm.mqjms.jar")
                .getAbsolutePath());
        webapp.addSharedClasspath(new File(wmqDir, "dhbcore.jar")
                .getAbsolutePath());

        webapp.setHome(System.getenv("CATALINA_HOME"));
        webapp.setOutput("target/cargo" + getName() + ".log");
        return webapp;
    }

    /**
     * Create a Tomcat resource for a WMQ queue.
     * 
     * @param jndiName the queue JNDI name
     * @param queueName the queue name (in WMQ)
     * @return a Tomact resource
     */
    @SuppressWarnings("unchecked")
    protected Resource getQueueResource(final String jndiName,
            final String queueName) {
        Resource dsQueue = new Resource(jndiName, "com.ibm.mq.jms.MQQueue");
        dsQueue.getParameters().put("baseQueueName", queueName);
        dsQueue.getParameters().put("targetClient", "1");
        return dsQueue;
    }
}

package com.legstar.proxy.invoke;

import junit.framework.TestCase;

import org.codehaus.cargo.container.InstalledLocalContainer;
import org.codehaus.cargo.container.configuration.LocalConfiguration;
import org.codehaus.cargo.container.deployable.Deployable;
import org.codehaus.cargo.container.deployable.WAR;
import org.codehaus.cargo.container.tomcat.Tomcat6xInstalledLocalContainer;
import org.codehaus.cargo.container.tomcat.Tomcat6xStandaloneLocalConfiguration;

/**
 * Used for test cases that needs access to a Web Service.
 * <p/>
 * Uses Cargo to run an embedded Tomcat with a deployed web service.
 * 
 */
public abstract class AbstractWebServiceTest extends TestCase {

    private InstalledLocalContainer webapp;

    public void setUp() {
        webapp = getContainer("target/wars/legstar-test-cultureinfo.war");
        webapp.start();

    }

    public void tearDown() {
        webapp.stop();
    }

    /**
     * Create a Cargo web container deploying a war in it.
     * 
     * @param warLocation the war location
     * @return a container ready to start
     */
    protected InstalledLocalContainer getContainer(final String warLocation) {
        Deployable war = new WAR(warLocation);
        LocalConfiguration configuration = new Tomcat6xStandaloneLocalConfiguration(
                "target/tomcat6x");
        configuration.addDeployable(war);
        InstalledLocalContainer webapp = new Tomcat6xInstalledLocalContainer(
                configuration);
        webapp.setHome(System.getenv("CATALINA_HOME"));
        webapp.setOutput("target/cargo.log");
        return webapp;
    }

}

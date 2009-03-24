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
package com.legstar.eclipse.plugin.jaxwsgen.preferences;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import com.legstar.cixs.jaxws.gen.Cixs2JaxwsGenerator;
import com.legstar.cixs.jaxws.gen.Jaxws2CixsGenerator;
import com.legstar.codegen.CodeGenUtil;
import com.legstar.eclipse.plugin.jaxwsgen.Activator;

/**
 * Class used to initialize default preference values.
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer {

    /**
     * {@inheritDoc}
     * @see org.eclipse.core.runtime.preferences.
     * AbstractPreferenceInitializer#initializeDefaultPreferences()
     */
    public void initializeDefaultPreferences() {
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();

        store.setDefault(PreferenceConstants.DEFAULT_J2EE_WAR_FOLDER,
                getDefaultJ2eeLocation() + "/webapps");

        store.setDefault(PreferenceConstants.DEFAULT_J2EE_WDD_FOLDER, "webapp/WEB-INF");

        store.setDefault(PreferenceConstants.ADAPTER_WSDL_TARGET_NAMESPACE_PREFIX,
                Jaxws2CixsGenerator.DEFAULT_WSDL_TARGET_NAMESPACE_PREFIX);
        store.setDefault(PreferenceConstants.ADAPTER_WSDL_SERVICE_NAME_SUFFIX,
                Jaxws2CixsGenerator.DEFAULT_WSDL_SERVICE_NAME_SUFFIX);
        store.setDefault(PreferenceConstants.ADAPTER_WSDL_PORT_NAME_SUFFIX,
                Jaxws2CixsGenerator.DEFAULT_WSDL_PORT_NAME_SUFFIX);

        store.setDefault(PreferenceConstants.PROXY_DEFAULT_HTTP_SCHEME, "http");

        store.setDefault(PreferenceConstants.PROXY_DEFAULT_HTTP_HOST, CodeGenUtil.getLocalIPAddress());

        store.setDefault(PreferenceConstants.PROXY_DEFAULT_HTTP_PORT, 8080);

        store.setDefault(PreferenceConstants.PROXY_HTTP_PATH_TEMPLATE,
                Cixs2JaxwsGenerator.DEFAULT_SERVER_PATH_TEMPLATE);

        store.setDefault(PreferenceConstants.COBOL_SAMPLE_FOLDER,
                "cobol");
    }

    /**
     * If Tomcat or Jetty are installed on this machine, this will retrieve the
     * installation folder.
     * @return the default location
     */
    public String getDefaultJ2eeLocation() {
        String value = System.getenv("CATALINA_BASE");
        if (value == null) {
            value = System.getenv("CATALINA_HOME");
            if (value == null) {
                value = System.getenv("JETTY_HOME");
                if (value == null) {
                    return "";
                }
            }
        }
        return value;
    }

}

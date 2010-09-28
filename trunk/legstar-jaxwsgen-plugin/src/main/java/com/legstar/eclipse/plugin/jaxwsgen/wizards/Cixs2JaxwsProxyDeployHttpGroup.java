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
package com.legstar.eclipse.plugin.jaxwsgen.wizards;

import com.legstar.cixs.gen.model.options.CobolHttpClientType;
import com.legstar.cixs.gen.model.options.HttpTransportParameters;
import com.legstar.eclipse.plugin.cixscom.wizards.AbstractCixsProxyDeployHttpGroup;
import com.legstar.eclipse.plugin.jaxwsgen.preferences.PreferenceConstants;

/**
 * Inherits the behavior of a group of controls for HTTP client parameters.
 * <p/>
 * The target is to manage the preferences at the local level rather the cixcom.
 * This is needed because the preferences should not be shared between execution
 * environments.
 */
public class Cixs2JaxwsProxyDeployHttpGroup extends
        AbstractCixsProxyDeployHttpGroup {

    /**
     * Construct this control group attaching it to a wizard page.
     * 
     * @param wizardPage the parent wizard page
     * @param genModel the data model
     * @param selected whether this group should initially be selected
     * @param sampleCobolHttpClientType initial HTTP sample client type
     */
    public Cixs2JaxwsProxyDeployHttpGroup(
            final Cixs2JaxwsGeneratorWizardPage wizardPage,
            final HttpTransportParameters genModel,
            final CobolHttpClientType sampleCobolHttpClientType,
            final boolean selected) {
        super(wizardPage, genModel, sampleCobolHttpClientType, selected);
    }

    /**
     * {@inheritDoc}
     */
    public String getDefaultHttpHost() {
        return getWizardPage().getStore().getString(
                PreferenceConstants.PROXY_DEFAULT_HTTP_HOST);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getDefaultHttpPort() {
        return getWizardPage().getStore().getInt(
                PreferenceConstants.PROXY_DEFAULT_HTTP_PORT);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDefaultHttpPath() {
        return getWizardPage().getStore().getString(
                PreferenceConstants.PROXY_HTTP_PATH_TEMPLATE).replace(
                        "${service.name}",
                getWizardPage().getServiceName());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDefaultHttpUserId() {
        return "";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDefaultHttpPassword() {
        return "";
    }

}

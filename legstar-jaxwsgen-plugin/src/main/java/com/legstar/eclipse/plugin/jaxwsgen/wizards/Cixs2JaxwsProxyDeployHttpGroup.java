package com.legstar.eclipse.plugin.jaxwsgen.wizards;

import com.legstar.eclipse.plugin.cixscom.wizards.AbstractCixsProxyDeployHttpGroup;
import com.legstar.eclipse.plugin.jaxwsgen.preferences.PreferenceConstants;

/**
 * Inherits the behavior of a group of controls for HTTP client parameters.
 * <p/>
 * The target is to manage the preferences at the local level rather the cixcom.
 * This is needed because the preferences should not be shared between execution
 * environments.
 */
public class Cixs2JaxwsProxyDeployHttpGroup extends AbstractCixsProxyDeployHttpGroup {

    /**
     * Construct this control group attaching it to a wizard page.
     * @param wizardPage the parent wizard page
     */
    public Cixs2JaxwsProxyDeployHttpGroup(final Cixs2JaxwsGeneratorWizardPage wizardPage) {
        super(wizardPage);
    }
    /**
     * {@inheritDoc} 
     */
    public void initExtendedControls() {

        setHttpHost(getProjectPreferences().get(PreferenceConstants.PROXY_LAST_HTTP_HOST,
                getWizardPage().getStore().getString(PreferenceConstants.PROXY_DEFAULT_HTTP_HOST)));
        
        setHttpPort(getProjectPreferences().get(PreferenceConstants.PROXY_LAST_HTTP_PORT,
                getWizardPage().getStore().getString(PreferenceConstants.PROXY_DEFAULT_HTTP_PORT)));

        setHttpPath(getProjectPreferences().get(PreferenceConstants.PROXY_LAST_HTTP_PATH,
                getWizardPage().getStore().getString(
                        PreferenceConstants.PROXY_HTTP_PATH_TEMPLATE).replace(
                                "${service.name}", getWizardPage().getServiceName())));
        
        getDfhwbcliButton().setSelection(getProjectPreferences().getBoolean(
                PreferenceConstants.PROXY_LAST_DFHWBCLI_BUTTON_SELECTION, true));
        getWebapiButton().setSelection(getProjectPreferences().getBoolean(
                PreferenceConstants.PROXY_LAST_WEBAPI_BUTTON_SELECTION, false));
        getLegstarButton().setSelection(getProjectPreferences().getBoolean(
                PreferenceConstants.PROXY_LAST_LEGSTAR_BUTTON_SELECTION, false));
    }

    /**
     * {@inheritDoc} 
     */
    public void storeExtendedProjectPreferences() {

        getProjectPreferences().put(PreferenceConstants.PROXY_LAST_HTTP_HOST, getHttpHost());
        getProjectPreferences().put(PreferenceConstants.PROXY_LAST_HTTP_PORT, getHttpPort());
        getProjectPreferences().put(PreferenceConstants.PROXY_LAST_HTTP_PATH, getHttpPath());

        getProjectPreferences().putBoolean(
                PreferenceConstants.PROXY_LAST_DFHWBCLI_BUTTON_SELECTION,
                getDfhwbcliButton().getSelection());
        getProjectPreferences().putBoolean(
                PreferenceConstants.PROXY_LAST_WEBAPI_BUTTON_SELECTION,
                getWebapiButton().getSelection());
        getProjectPreferences().putBoolean(
                PreferenceConstants.PROXY_LAST_LEGSTAR_BUTTON_SELECTION,
                getLegstarButton().getSelection());
    }

}

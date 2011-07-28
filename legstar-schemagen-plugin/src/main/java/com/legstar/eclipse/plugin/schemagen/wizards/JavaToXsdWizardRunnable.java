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
package com.legstar.eclipse.plugin.schemagen.wizards;

import java.lang.reflect.InvocationTargetException;

import com.legstar.eclipse.plugin.common.wizards.AbstractWizard;
import com.legstar.xsd.java.Java2CobModel;

/**
 * Background task that performs the actual XSD generation.
 */
public class JavaToXsdWizardRunnable extends AbstractToXsdWizardRunnable {

    /**
     * Instantiate this runnable from UI items. It is important not to attempt
     * access to UI elements from the background thread.
     * 
     * @param mainPage the page holding targets
     * @param javaToXsdPage the java classes page
     * @throws InvocationTargetException if runnable cannot be instantiated
     */
    public JavaToXsdWizardRunnable(final MainWizardPage mainPage,
            final JavaToXsdWizardPage javaToXsdPage)
            throws InvocationTargetException {
        super(null, mainPage);
        setAntBuildModel(getModel(mainPage, javaToXsdPage));
    }

    /**
     * Create a model ready to be passed to velocity for ant script generation.
     * 
     * @param mainPage wizard page holding target XSD location and parameters
     * @param javaToXsdPage the wizard page holding selected java classes
     * @return a valid model
     * @throws InvocationTargetException if model cannot be built
     */
    protected Java2CobModel getModel(final MainWizardPage mainPage,
            final JavaToXsdWizardPage javaToXsdPage)
            throws InvocationTargetException {
        Java2CobModel model = new Java2CobModel();
        model.setProductLocation(AbstractWizard
                .getPluginInstallLocation(com.legstar.eclipse.plugin.common.Activator.PLUGIN_ID));
        model.setClassNames(javaToXsdPage.getSelectedClassNames());
        model.setPathElementLocations(javaToXsdPage
                .getSelectedPathElementsLocations());

        model.setNewTargetNamespace(mainPage.getTargetNamespace());
        model.setTargetXsdFile(getTargetXsdFile(mainPage.getTargetXSDFileName()));
        model.setTargetCobolFile(getTargetCobolFile(mainPage
                .getTargetXSDFileName()));
        return model;
    }

}

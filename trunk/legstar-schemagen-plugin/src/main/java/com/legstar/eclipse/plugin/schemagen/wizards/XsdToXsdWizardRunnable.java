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
package com.legstar.eclipse.plugin.schemagen.wizards;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import com.legstar.xsdc.gen.XsdToXsdCobolModel;

/**
 * Background task that performs the actual XSD generation.
 */
public class XsdToXsdWizardRunnable extends AbstractToXsdWizardRunnable {

    /** Pattern for temporary files. */
    private static final String TEMP_PATTERN = "legstar-schemagen";

    /** Suffix for temporary files. */
    private static final String TEMP_SUFFIX = ".temp";

    /**
     * Instantiate this runnable from UI items. It is important not to attempt
     * access to UI elements from the background thread.
     * 
     * @param mainPage the page holding targets
     * @param xsdToXsdPage the XML schema page
     * @throws InvocationTargetException if runnable cannot be instantiated
     */
    public XsdToXsdWizardRunnable(
            final MainWizardPage mainPage,
            final XsdToXsdWizardPage xsdToXsdPage)
            throws InvocationTargetException {
        super(null, mainPage);
        setAntBuildModel(getModel(mainPage, xsdToXsdPage));
    }

    /**
     * Create a model ready to be passed to velocity for ant script generation.
     * 
     * @param mainPage wizard page holding target XSD location and parameters
     * @param xsdToXsdPage the wizard page holding selected Xml schemas or Wsdl
     * @return a valid model
     * @throws InvocationTargetException if model cannot be built
     */
    protected XsdToXsdCobolModel getModel(
            final MainWizardPage mainPage,
            final XsdToXsdWizardPage xsdToXsdPage)
            throws InvocationTargetException {
        XsdToXsdCobolModel model = new XsdToXsdCobolModel();
        model.setProductLocation(getPluginInstallLocation(
                com.legstar.eclipse.plugin.common.Activator.PLUGIN_ID));

        /* Store the content of the text box in a temporary file */
        File xsdFile;
        try {
            xsdFile = File.createTempFile(TEMP_PATTERN, TEMP_SUFFIX);
            BufferedWriter writer = new BufferedWriter(new FileWriter(xsdFile));
            writer.write(xsdToXsdPage.getXsdSourceText().getText());
            writer.close();
        } catch (IOException e) {
            throw new InvocationTargetException(e);
        }

        /* Temporary file becomes input to generation process */
        model.setInputXsdUri(xsdFile.toURI());

        if (xsdToXsdPage.isSwitchNamespaceAllowed()) {
            model.setNamespace(mainPage.getTargetNamespace());
        }
        model.setTargetDir(new File(getTargetXsdLocation()));
        model.setTargetXsdFileName(mainPage.getTargetXSDFileName());
        return model;
    }

}

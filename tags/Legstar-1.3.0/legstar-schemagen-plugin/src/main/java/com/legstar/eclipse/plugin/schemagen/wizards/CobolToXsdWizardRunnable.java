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

import org.eclipse.jface.preference.IPreferenceStore;

import com.legstar.cob2xsd.task.CobolStructureToXsdModel;
import com.legstar.eclipse.plugin.schemagen.Activator;
import com.legstar.eclipse.plugin.schemagen.preferences.PreferenceConstants;

/**
 * Background task that performs the actual XSD generation.
 */
public class CobolToXsdWizardRunnable extends AbstractToXsdWizardRunnable {

    /** Pattern for temporary files. */
    private static final String TEMP_PATTERN = "legstar-schemagen";

    /** Suffix for temporary files. */
    private static final String TEMP_SUFFIX = ".temp";

    /**
     * Instantiate this runnable from UI items. It is important not to attempt
     * access to UI elements from the background thread.
     * @param mainPage the page holding targets
     * @param cobolToXsdPage the cobol page
     * @throws InvocationTargetException if runnable cannot be instantiated
     */
    public CobolToXsdWizardRunnable(
            final MainWizardPage mainPage,
            final CobolToXsdWizardPage cobolToXsdPage)
    throws InvocationTargetException {
        super(null, mainPage);
        setAntBuildModel(getModel(mainPage, cobolToXsdPage));
    }

    /**
     * Create a model ready to be passed to velocity for ant script generation.
     * @param mainPage wizard page holding target XSD location and parameters
     * @param cobolToXsdPage the wizard page holding selected COBOL fragment
     * @return a valid model
     * @throws InvocationTargetException if model cannot be built
     */
    protected CobolStructureToXsdModel getModel(
            final MainWizardPage mainPage,
            final CobolToXsdWizardPage cobolToXsdPage)
    throws InvocationTargetException {

    	CobolStructureToXsdModel model = new CobolStructureToXsdModel();
        model.setProductLocation(getPluginInstallLocation(
                com.legstar.eclipse.plugin.common.Activator.PLUGIN_ID));

        /* Store the content of the text box in a temporary file */
        File cobolFile;
        try {
            cobolFile = File.createTempFile(TEMP_PATTERN, TEMP_SUFFIX);
            BufferedWriter writer = new BufferedWriter(
                    new FileWriter(cobolFile));
            writer.write(cobolToXsdPage.getCobolFragment());
            writer.close();
        } catch (IOException e) {
            throw new InvocationTargetException(e);
        }

        /* Temporary file becomes input to generation process */
        model.setCobolSourceFilePath(cobolFile.getPath());
        model.setTargetDir(new File(getTargetXsdLocation()));
        model.setTargetXsdFileName(mainPage.getTargetXSDFileName());

        IPreferenceStore store = Activator.getDefault().getPreferenceStore();

        model.getContext().setXsdEncoding(store.getString(PreferenceConstants.DEFAULT_XSD_ENCODING));
        model.getContext().setTargetNamespace(mainPage.getTargetNamespace());
        model.getContext().setMapConditionsToFacets(store.getBoolean(PreferenceConstants.DEFAULT_XSD_MAP_CONDITIONS_TO_FACETS));
        model.getContext().setCustomXsltFileName(store.getString(PreferenceConstants.DEFAULT_XSD_CUSTOM_XSLT_FILE_NAME));
        model.getContext().setNameConflictPrependParentName(store.getBoolean(PreferenceConstants.DEFAULT_XSD_NAME_CONFLICT_PREPEND_PARENT_NAME));
        model.getContext().setElementNamesStartWithUppercase(store.getBoolean(PreferenceConstants.DEFAULT_XSD_ELEMENT_NAMES_START_WITH_UPPERCASE));

        model.getContext().setAddLegStarAnnotations(store.getBoolean(PreferenceConstants.DEFAULT_ADD_LEGSTAR_ANNOTATIONS));
        model.getContext().setJaxbPackageName(mainPage.getTargetJaxbPackageName());
        model.getContext().setJaxbTypeClassesSuffix(store.getString(PreferenceConstants.DEFAULT_JAXB_TYPE_CLASSES_SUFFIX));
        
        model.getContext().setCurrencySymbol(store.getString(PreferenceConstants.DEFAULT_CURRENCY_SYMBOL));
        model.getContext().setDecimalPointIsComma(store.getBoolean(PreferenceConstants.DEFAULT_DECIMAL_POINT_IS_COMMA));
        model.getContext().setNSymbolDbcs(store.getBoolean(PreferenceConstants.DEFAULT_NSYMBOL_DBCS));
        model.getContext().setQuoteIsQuote(store.getBoolean(PreferenceConstants.DEFAULT_QUOTE_IS_QUOTE));
        return model;
        
    }

}

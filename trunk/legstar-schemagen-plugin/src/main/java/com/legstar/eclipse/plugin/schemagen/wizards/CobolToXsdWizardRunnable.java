package com.legstar.eclipse.plugin.schemagen.wizards;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import com.legstar.schemagen.CobolToXsdCobolModel;

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
    protected CobolToXsdCobolModel getModel(
            final MainWizardPage mainPage,
            final CobolToXsdWizardPage cobolToXsdPage)
            throws InvocationTargetException {

    	CobolToXsdCobolModel model = new CobolToXsdCobolModel();
        model.setProductLocation(getPluginInstallLocation());
        
        /* Store the content of the text box in a temporary file */
        File cobolFile;
        try {
            cobolFile = File.createTempFile(TEMP_PATTERN, TEMP_SUFFIX);
            BufferedWriter writer = new BufferedWriter(
            		new FileWriter(cobolFile));
            writer.write(cobolToXsdPage.getCobolFragmentText().getText());
            writer.close();
        } catch (IOException e) {
            throw new InvocationTargetException(e);
        }

        /* Temporary file becomes input to generation process */
        model.setSourceCobolFilePath(cobolFile.getPath());
         
        model.setJaxbPackageName(
                mainPage.getTargetJaxbPackageNameText().getText());
        model.setNamespace(mainPage.getTargetNamespaceText().getText());
        model.setTargetDir(new File(getTargetXsdLocation()));
        model.setTargetXsdFileName(mainPage.getTargetFileText().getText());
        return model;
    }

}

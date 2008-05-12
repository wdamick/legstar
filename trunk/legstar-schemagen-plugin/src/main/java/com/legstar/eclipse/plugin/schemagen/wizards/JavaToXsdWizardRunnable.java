package com.legstar.eclipse.plugin.schemagen.wizards;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import com.legstar.j2sc.gen.JavaToXsdCobolModel;

/**
 * Background task that performs the actual XSD generation.
 */
public class JavaToXsdWizardRunnable extends AbstractWizardRunnable {

     /**
     * Instantiate this runnable from UI items. It is important not to attempt
     * access to UI elements from the background thread.
     * @param mainPage the page holding targets
     * @param javaToXsdPage the java classes page
     * @throws InvocationTargetException if runnable cannot be instantiated
     */
    public JavaToXsdWizardRunnable(
            final MainWizardPage mainPage,
            final JavaToXsdWizardPage javaToXsdPage)
            throws InvocationTargetException {
        super(null, mainPage);
        setAntBuildModel(getModel(mainPage, javaToXsdPage));
    }

    /**
     * Create a model ready to be passed to velocity for ant script generation.
     * @param mainPage wizard page holding target XSD location and parameters
     * @param javaToXsdPage the wizard page holding selected java classes
     * @return a valid model
     * @throws InvocationTargetException if model cannot be built
     */
    protected JavaToXsdCobolModel getModel(
            final MainWizardPage mainPage,
            final JavaToXsdWizardPage javaToXsdPage)
            throws InvocationTargetException {
        JavaToXsdCobolModel model = new JavaToXsdCobolModel();
        model.setProductLocation(getPluginInstallLocation());
        model.setClassNames(javaToXsdPage.getSelectedClassNames());
        model.setPathElementLocations(
                javaToXsdPage.getSelectedPathElementsLocations());

        model.setJaxbPackageName(
                mainPage.getTargetJaxbPackageNameText().getText());
        model.setNamespace(mainPage.getTargetNamespaceText().getText());
        model.setTargetDir(new File(getTargetXsdLocation()));
        model.setTargetXsdFileName(mainPage.getTargetFileText().getText());
        return model;
    }

}

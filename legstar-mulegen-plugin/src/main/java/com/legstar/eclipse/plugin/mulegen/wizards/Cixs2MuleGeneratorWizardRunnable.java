package com.legstar.eclipse.plugin.mulegen.wizards;

import java.io.File;
import java.lang.reflect.InvocationTargetException;

import org.mule.providers.legstar.model.AntBuildCixs2MuleModel;
import org.mule.providers.legstar.model.CixsMuleComponent;

import com.legstar.cixs.gen.ant.model.AbstractAntBuildCixsModel;
import com.legstar.cixs.gen.model.AbstractCixsService;
import com.legstar.eclipse.plugin.cixscom.wizards
		.AbstractCixsGeneratorWizardPage;
import com.legstar.eclipse.plugin.cixscom.wizards
		.AbstractCixsGeneratorWizardRunnable;

/**
 * Background task that performs the actual artifacts generation. The process
 * involves 2 steps:
 * <ul>
 *  <li>Build an ant script file using a velocity template</li>
 *  <li>Launch the ant script as a background process</li>
 * </ul>
 */
public class Cixs2MuleGeneratorWizardRunnable
		extends AbstractCixsGeneratorWizardRunnable {
	
	/** Part of the ant script file name generated. Allows segregating
	 * this ant file from the ones produced by other LegStar wizards. */
	private static final String ANT_FILE_NAME_ID = "mule-c2m-";

	/**
	 * Constructs the backend generation task. 
	 * The ant script will be generated under the folder from
	 * preferences.
	 * TODO allow user to select the ant script location
	 * @param cixs2MuleGenWizardPage the main wizard page
	 * @throws InvocationTargetException if construction fails
	 */
	public Cixs2MuleGeneratorWizardRunnable(
			final Cixs2MuleGeneratorWizardPage cixs2MuleGenWizardPage)
			throws InvocationTargetException {
		super(cixs2MuleGenWizardPage, ANT_FILE_NAME_ID);
	}

    /**
     * Create a model ready to be passed to velocity for ant script generation.
     * @param cixsGenWizardPage the wizard page holding input parameters
     * @return a valid model
     * @throws InvocationTargetException if model cannot be built
     */
    protected AbstractAntBuildCixsModel getGenerationModel(
            final AbstractCixsGeneratorWizardPage cixsGenWizardPage)
            throws InvocationTargetException {
        AntBuildCixs2MuleModel genModel = new AntBuildCixs2MuleModel();
        Cixs2MuleGeneratorWizardPage page =
            (Cixs2MuleGeneratorWizardPage) cixsGenWizardPage;
        setModel(page, genModel);
        genModel.setMuleHome(page.getMuleHome());
        genModel.getCixsMuleComponent().setImplementationClassName(
                page.getMuleUMOImplementation());
        genModel.getCixsMuleComponent().setServiceURI(page.getServiceURI());
        genModel.setTargetCobolDir(new File(page.getTargetCobolDir()));
        genModel.setTargetJarDir(new File(page.getTargetJarDir()));
        genModel.setTargetMuleConfigDir(
        		new File(page.getTargetMuleConfigDir()));
      return genModel;
    }
	
    /** {@inheritDoc} */
    public AbstractCixsService createCixsService() {
        return new CixsMuleComponent();
    }
}

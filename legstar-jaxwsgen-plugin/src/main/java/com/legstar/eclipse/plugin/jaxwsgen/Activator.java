package com.legstar.eclipse.plugin.jaxwsgen;

import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceRegistration;

import com.legstar.eclipse.plugin.cixscom.wizards.AbstractCixsActivator;
import com.legstar.eclipse.plugin.jaxwsgen.wizards
					.Jaxws2CixsGeneratorWizardLauncher;

/**
 * This generator plugin register itself for dynamic discovery.
 * @see com.legstar.eclipse.plugin.cixscom.wizards.AbstractCixsActivator.
 */
public class Activator extends AbstractCixsActivator  {

	/** The plug-in ID. */
	public static final String PLUGIN_ID =
		"com.legstar.eclipse.plugin.jaxwsgen";

	/** The shared instance. */
	private static Activator mPlugin;
	
	/** The result of registering a Jaxws2Cixs generator service. */
	private ServiceRegistration mJaxws2CixsGeneratorService;
	
	/**
	 * Default constructor.
	 */
	public Activator() {
		super(PLUGIN_ID);
	}
	
	/**
	 * {@inheritDoc}
	 */
	public void start(final BundleContext context) throws Exception {
		super.start(context);
		mPlugin = this;
		mJaxws2CixsGeneratorService = 
			Jaxws2CixsGeneratorWizardLauncher.register(context);
	}

	/**
	 * {@inheritDoc}
	 */
	public void stop(final BundleContext context) throws Exception {
		if (mJaxws2CixsGeneratorService != null) {
			mJaxws2CixsGeneratorService.unregister();
		}
		mPlugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance.
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return mPlugin;
	}

}

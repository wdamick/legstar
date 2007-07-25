package com.legstar.c2ws;

import java.io.File;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.configuration.AbstractFileConfiguration;
import org.apache.commons.configuration.CombinedConfiguration;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.DefaultConfigurationBuilder;
import org.apache.commons.configuration.event.ConfigurationEvent;
import org.apache.commons.configuration.event.ConfigurationListener;
import org.apache.commons.configuration.reloading.FileChangedReloadingStrategy;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Each Web Service Descriptor is an XML file used as an Apache XMLConfiguration
 * object. A combined configuration lists these available descriptors in its
 * &lt;additional&gt; section (see Apache Combined Configurations).
 * This manager will ensure that the combined configuration file gets reloaded
 * when new web service descriptors are added (or removed).
 */
public class C2wsConfigurationManager implements ConfigurationListener {
	
	/** This Apache configuration class manages the combined configuration
	 * that result from adding each web service descriptor as an individual
	 * configuration. */
	private DefaultConfigurationBuilder mC2wsBuilder;
	
	/** The combined configuration object associated with the configuration
	 * builder. */
	private CombinedConfiguration mC2wsConfig;
	
	/** This indicates if the combined configuration needs to be reconstructed
	 * from the builder. This situation happens when the underlying
	 * configuration file changes (web services are added or removed). */
	private boolean mC2wsConfigDirty;

	/** The file name where the combined web service descriptors configuration
	 * is described. */
	private String mC2wsConfigFileName;
	
	/** This structure is used to cache the web service descriptors because
	 * it is expensive to create new ones. */
	private Map < String, C2wsWSDescriptor > mWsdCache;
	
	/** Logger. */
	private static final Log LOG =
		LogFactory.getLog(C2wsConfigurationManager.class);

	/**
	 * Constructor from a configuration file name.
	 * @param c2wsConfigFileName the configuration file expected to be located
	 * in the classpath
	 * @throws C2wsConfigurationException if configuration cannot be built
	 */
	public C2wsConfigurationManager(
			final String c2wsConfigFileName) throws C2wsConfigurationException {
		if (c2wsConfigFileName == null || c2wsConfigFileName.length() == 0) {
			throw new C2wsConfigurationException("No configuration file name");
		}
		mC2wsBuilder = new DefaultConfigurationBuilder();
		mC2wsBuilder.addConfigurationListener(this);
		mC2wsConfigFileName = c2wsConfigFileName;
		mC2wsConfig = loadC2wsConfig(c2wsConfigFileName);
		mC2wsConfigDirty = false;
		mWsdCache = new HashMap < String, C2wsWSDescriptor >();
	}
	
	/**
	 * Retrieves the descriptor for the target web service. Because descriptors
	 * keep being added to the combined configuration file, we give the builder
	 * a chance to reload itself on each call to this method.
	 * @param serviceName the target web service identifier
	 * @return a target web service descriptor
	 * @throws C2wsConfigurationException if service has no descriptors
	 */
	public final synchronized C2wsWSDescriptor getWebServiceDescriptor(
			final String serviceName) throws C2wsConfigurationException {
		
		/* The reload method checks the file last modified date and actually
		 * reloads only if a change is detected. If a reload occurs a change
		 * event is triggered (see configurationChanged method)*/
		mC2wsBuilder.reload();
		if (mC2wsConfigDirty) {
			LOG.info("Reloading the " + mC2wsConfigFileName
					+ " configuration file.");
        	try {
				mC2wsConfig = mC2wsBuilder.getConfiguration(false);
			} catch (ConfigurationException e) {
				throw new C2wsConfigurationException(e);
			}
        	mC2wsConfigDirty = false;
        	
        	/* Empty the cache since its content may now be out of sync */
    		mWsdCache = new HashMap < String, C2wsWSDescriptor >();
		} else {
			/* try to locate the descriptor in the cache */
			C2wsWSDescriptor wsd = mWsdCache.get(serviceName);
			if (wsd != null) {
				if (LOG.isDebugEnabled()) {
					LOG.debug("Cached configuration for "
							+ serviceName + " is:");
					LOG.debug(wsd.toString());
				}
				return wsd;
			}
			
		}
		
		CombinedConfiguration ccAdd = (CombinedConfiguration)
		  mC2wsConfig.getConfiguration(
				  DefaultConfigurationBuilder.ADDITIONAL_NAME);
		Configuration wsConfig = null;
		if (ccAdd != null) {
			wsConfig = ccAdd.getConfiguration(serviceName);
		}
		if (wsConfig == null) {
			throw new C2wsConfigurationException("Service " + serviceName
					+ " has no descriptors in " + mC2wsConfigFileName);
		}
		C2wsWSDescriptor wsd = new C2wsWSDescriptor(wsConfig);
		
		/* cache this descriptor for later use */
		mWsdCache.put(serviceName, wsd);
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("New configuration for " + serviceName + " is:");
			LOG.debug(wsd.toString());
		}
		return wsd;
	}
	
	/**
	 * Initial load of the combined configuration data file that lists
	 * available Web Service descriptors. Each decriptor is itself a
	 * configuration file.
	 * @param c2wsConfigResourceName the config resource name
	 * @return the configuration
	 * @throws C2wsConfigurationException if configuration cannot be loaded
	 */
	private CombinedConfiguration loadC2wsConfig(
			final String c2wsConfigResourceName)
			throws C2wsConfigurationException {

		LOG.info("Loading the " + c2wsConfigResourceName
				+ " configuration file.");
		
		if (c2wsConfigResourceName == null
				|| c2wsConfigResourceName.length() == 0) {
			throw new C2wsConfigurationException(
					"No configuration resource name was provided");
		}
		URL resource = C2wsConfigurationManager.class.getResource(
				(c2wsConfigResourceName.charAt(0) == '/')
				? c2wsConfigResourceName
				: "/" + c2wsConfigResourceName);
		if (resource == null) {
			throw new C2wsConfigurationException(
					"Unable to locate resource " + c2wsConfigResourceName);
		}
		/* If this is actually a file, we can use the commons config reload
		 * strategy. */
		String externalForm = resource.toExternalForm();
		if (externalForm.startsWith("file:")) {
			if (LOG.isDebugEnabled()) {
				LOG.debug("Loading config from file "
						+ externalForm.substring(6));
			}
			mC2wsBuilder.setFile(new File(externalForm.substring(5)));
			mC2wsBuilder.setReloadingStrategy(
					new FileChangedReloadingStrategy());
		} else {
			if (LOG.isDebugEnabled()) {
				LOG.debug("Loading config from URL "
						+ resource);
			}
			mC2wsBuilder.setURL(resource);
		}

		try {
			CombinedConfiguration config = mC2wsBuilder.getConfiguration(true);
			return config;
		} catch (ConfigurationException e) {
			throw new C2wsConfigurationException(e);
		}
	}

	/**
	 * @return the c2ws configuration
	 */
	public final Configuration getC2wsConfig() {
		return mC2wsConfig;
	}

	/**
	 * @return the current configuration file nale
	 */
	public final String getC2wsConfigFileName() {
		return mC2wsConfigFileName;
	}

	/**
	 * In case of a reload event, we reconstruct the combined configuration.
	 * {@inheritDoc}}
	 * */
	public final void configurationChanged(final ConfigurationEvent event) {
        if (!event.isBeforeUpdate()
        		&& event.getType() == AbstractFileConfiguration.EVENT_RELOAD) {
        	mC2wsConfigDirty = true;
        }
	}
}

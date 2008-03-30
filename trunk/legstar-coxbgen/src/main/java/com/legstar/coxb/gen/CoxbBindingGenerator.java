/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.coxb.gen;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.BuildException;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.host.HostException;
import com.sun.xml.bind.api.impl.NameConverter;

import java.io.File;
import java.io.IOException;
import java.net.URL;

import org.w3c.dom.Element;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;


import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.ParserConfigurationException;

import org.xml.sax.SAXException;

/**
 * This class implements an ant task to generate COXB binding data from
 * JAXB cobol annotated instances. The generated binding code is faster than
 * alternative reflection methods also available in <code>legstar-coxbrt</code>.
 * Another advantage of binding classes is that they can bind to a different
 * object than the original JAXB object which served to generate it.
 * 
 * @author Fady Moussallam
 * 
 */
public class CoxbBindingGenerator extends Task {

	/** The JAXB namespace needed to retrieve jaxb elements from XML schema. */
	private static final String JAXB_NS = "http://java.sun.com/xml/ns/jaxb";
	
	/** The JAXB package element name. */
	private static final String JAXB_PACKAGE_LN = "package";
	
	/** The JAXB package name attribute. */
	private static final String JAXB_PACKAGE_ATTR = "name";
	
	/** Container for all parameters to move around. */
	private CoxbGenContext mCoxbGenContext = new CoxbGenContext();
	
	/** Logger. */
	private static final Log LOG =
		LogFactory.getLog(CoxbBindingGenerator.class);

	/**
	 *  The ant method. Generates COXB binding code.
	 */
    public final void execute() {
    	checkInput();
    	
    	/* If we are not provided with a physical location for JAXB classes,
    	 * assume they are available from the current classpath. */
    	Object jaxbObjectFactory;
    	if (getJaxbDir() == null ||	getJaxbDir().length() == 0) {
    		jaxbObjectFactory = getObjectFactory(getJaxbPackageName());
    	} else {
    	/* Create an instance of the JAXB object factory */
    		jaxbObjectFactory = getObjectFactory(
    				getJaxbPackageName(), getJaxbDir());
    	}
    	
    	/* Create an instance of the JAXB root object */
    	Object jaxbRootObject = getRootObject(
                jaxbObjectFactory, getJaxbRootObjectName());
    	
    	try {
        	/* Create a visitor */
        	CoxbGenReflectVisitor visitor =
        		new CoxbGenReflectVisitor(mCoxbGenContext);
        	/* Bind the root object to a COXB type */
        	CComplexReflectBinding ce = new CComplexReflectBinding(
                    jaxbObjectFactory, jaxbRootObject);
			ce.accept(visitor);
		} catch (HostException e) {
			e.printStackTrace();
			throw (new BuildException(
					"HostException " + e.getMessage()));
		}
 
    }
    
	/**
	 * Checks that properties set are valid.
	 */
	private void checkInput() {
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("checkInput started");
			mCoxbGenContext.traceContext();
		}
		
    	/* If user did not provide a JAXB package name, we need to get it
    	 * from the XML schema annotations.  */
    	if (getJaxbPackageName() == null 
    			|| getJaxbPackageName().length() == 0) {
        	if (getXsdFile() == null || !getXsdFile().exists()) {
    			throw (new BuildException(
    					"You must specify either a JAXB package name or"
    					 + " an XML schema file name"));
        	}
        	setJaxbPackageName(getPackageName(getXsdFile()));
    	}

    	if (getTargetDir() == null || !getTargetDir().exists()) {
			throw (new BuildException("You must specify a target directory"));
    	}
    	if (getJaxbRootObjectName() == null
    			|| getJaxbRootObjectName().length() == 0) {
			throw (new BuildException(
                    "You must specify a JAXB root object name"));
    	}
 		
	}
    
    /**
     * Loads the object factory class from a physical location and returns a 
     * new instance of it.
     *
     * @param jaxbPackageName the JAXB package name
     * @param jaxbDir the location of JAXB classes
     * @return a JAXB Object factory
     */
    public static Object getObjectFactory(
    		final String jaxbPackageName,
    		final File jaxbDir) {
    	
    	Object jaxbObjectFactory = null;
    	
        try {
            URL url = jaxbDir.toURI().toURL();
            URL[] urls = new URL[]{url};
        
            /* Create a new class loader with the directory */
            ClassLoader cl = new java.net.URLClassLoader(urls);
        
            /* Load in ObjectFactory.class */
            Class < ? > objfCls = cl.loadClass(
            		jaxbPackageName + ".ObjectFactory");
            
			/* Create a new instance of this class */
			jaxbObjectFactory = objfCls.newInstance();
			
        } catch (java.net.MalformedURLException e) {
			throw (new BuildException(
					"MalformedURLException " + e.getMessage()));
        } catch (ClassNotFoundException e) {
			throw (new BuildException(
					"ClassNotFoundException " + e.getMessage()
					+ " in " + jaxbDir));
		} catch (InstantiationException e) {
			e.printStackTrace();
			throw (new BuildException(
					"InstantiationException " + e.getMessage()));
		} catch (IllegalAccessException e) {
			e.printStackTrace();
			throw (new BuildException(
					"IllegalAccessException " + e.getMessage()));
       }
		
		return jaxbObjectFactory;
    }
    
    /**
     * Loads the object factory class using the current class loader assuming
     * the JAXB classes are available in the current classpath and returns a
     * new instance of it.
     *
     * @param packageName the package containing a JAXB Object Factory
     * @return a JAXB Object factory
     */
    private Object getObjectFactory(
    		final String packageName) {
    	
    	Object jaxbObjectFactory = null;
    	
    	if (packageName == null || packageName.length() == 0) {
			throw (new BuildException(
					"You must provide a JAXB package name."));
    	}
    	
		try {
			/* Get the JAXB ObjectFactory class */
			Class < ? > objfCls = this.getClass().getClassLoader().loadClass(
                    packageName + ".ObjectFactory");
			/* Create a new instance of this class */
			jaxbObjectFactory = objfCls.newInstance();
		} catch (ClassNotFoundException e) {
			throw new BuildException(e);
		} catch (InstantiationException e) {
			throw new BuildException(e);
		} catch (IllegalAccessException e) {
			throw new BuildException(e);
		} catch (SecurityException e) {
			throw new BuildException(e);
		} catch (IllegalArgumentException e) {
			throw new BuildException(e);
		}
		
		return jaxbObjectFactory;
    }

    /**
     * Extracts the JAXB package name from the XML schema annotations.
     * 
     * @param xsdFile the XML schema file
     * @return the package name
     */
    public static String getPackageName(final File xsdFile) {
    	String packageName = null;
    	DocumentBuilderFactory docBuilderFactory =
    		DocumentBuilderFactory.newInstance();
    	DocumentBuilder docBuilder;
		try {
			docBuilderFactory.setNamespaceAware(true);
			docBuilder = docBuilderFactory.newDocumentBuilder();
			Document doc = docBuilder.parse(xsdFile);
			NodeList listOfElements = doc.getElementsByTagNameNS(JAXB_NS,
					JAXB_PACKAGE_LN);
			if (listOfElements == null || listOfElements.getLength() == 0) {
				throw (new BuildException(
						"No JAXB annotations in XML schema file"));
			}
			packageName = ((Element) listOfElements.item(0)).getAttribute(
					JAXB_PACKAGE_ATTR);

		} catch (ParserConfigurationException e) {
			throw (new BuildException(
					"ParserConfigurationException " + e.getMessage()));
		} catch (SAXException e) {
			throw (new BuildException(
					"SAXException " + e.getMessage()));
		} catch (IOException e) {
			throw (new BuildException(
					"IOException " + e.getMessage()));
		}
    	return packageName;
    }

    /**
     * Returns a new instance of the requested JAXB object.
     *
     * @param jaxbObjectFactory an instance of a JAXB Object Factory
     * @param rootObjectName the JAXB root object name (non qualified)
     * @return an instance of the JAXB root object
     */
    private Object getRootObject(
    		final Object jaxbObjectFactory,
    		final String rootObjectName) {
    	
    	Object jaxbRootObject = null;
    	
    	if (jaxbObjectFactory == null) {
			throw (new BuildException(
					"You must provide a JAXB object factory."));
    	}
    	
    	if (rootObjectName == null || rootObjectName.length() == 0) {
			throw (new BuildException(
					"You must provide a JAXB object name."));
    	}
    	
		try {
			/* Here we make sure the object name is following the
			 * jaxb naming conventions. */
			String createName = "create"
				+ NameConverter.standard.toClassName(rootObjectName);
			Method creator = jaxbObjectFactory.getClass().getMethod(createName);
			jaxbRootObject = creator.invoke(jaxbObjectFactory);
		} catch (IllegalAccessException e) {
			e.printStackTrace();
			throw (new BuildException(
					"IllegalAccessException " + e.getMessage()
					+ rootObjectName));
		} catch (SecurityException e) {
			e.printStackTrace();
			throw (new BuildException(
					"SecurityException " + e.getMessage()
					+ rootObjectName));
		} catch (NoSuchMethodException e) {
			e.printStackTrace();
			throw (new BuildException(
					"NoSuchMethodException " + e.getMessage()
					+ rootObjectName));
		} catch (IllegalArgumentException e) {
			e.printStackTrace();
			throw (new BuildException(
					"IllegalArgumentException " + e.getMessage()
					+ rootObjectName));
		} catch (InvocationTargetException e) {
			e.printStackTrace();
			throw (new BuildException(
					"InvocationTargetException " + e.getMessage()
					+ rootObjectName));
		}
		
		return jaxbRootObject;
    }

	/**
	 * @return Returns the JAXB root object name.
	 */
	public final String getJaxbRootObjectName() {
		return mCoxbGenContext.getJaxbRootObjectName();
	}

	/**
	 * @param objectName The JAXB root object name to set.
	 */
	public final void setJaxbRootObjectName(
			final String objectName) {
		mCoxbGenContext.setJaxbRootObjectName(objectName);
	}

	/**
	 * @return the XML schema file
	 */
	public final File getXsdFile() {
		return mCoxbGenContext.getXsdFile();
	}

	/**
	 * @param xsdFile the XML schema file to set
	 */
	public final void setXsdFile(
			final File xsdFile) {
		mCoxbGenContext.setXsdFile(xsdFile);
	}

	/**
	 * @return the current target directory
	 */
	public final File getTargetDir() {
		return mCoxbGenContext.getTargetDir();
	}

	/**
	 * @param targetDir the target directory to set
	 */
	public final void setTargetDir(final File targetDir) {
		mCoxbGenContext.setTargetDir(targetDir);
	}

	/**
	 * @return the location where JAXB classes live
	 */
	public final File getJaxbDir() {
		return mCoxbGenContext.getJaxbDir();
	}

	/**
	 * @param jaxbDir the JAXB location to set
	 */
	public final void setJaxbDir(final File jaxbDir) {
		mCoxbGenContext.setJaxbDir(jaxbDir);
	}

	/**
	 * @return the package name used for JAXB classes
	 */
	public final String getJaxbPackageName() {
		return mCoxbGenContext.getJaxbPackageName();
	}

	/**
	 * @param jaxbPackageName the JAXB classes package name to set
	 */
	public final void setJaxbPackageName(final String jaxbPackageName) {
		mCoxbGenContext.setJaxbPackageName(jaxbPackageName);
	}

	/**
	 * @return the package name for generated binding classes
	 */
	public final String getCoxbPackageName() {
		return mCoxbGenContext.getCoxbPackageName();
	}

	/**
	 * @param coxbPackageName package name for generated binding classes to set
	 */
	public final void setCoxbPackageName(final String coxbPackageName) {
		mCoxbGenContext.setCoxbPackageName(coxbPackageName);
	}

	/**
	 * @return the optional runtime alternative to the Jaxb package name used at
	 * generation time
	 */
	public final String getAlternativePackageName() {
		return mCoxbGenContext.getAlternativePackageName();
	}

	/**
	 * @param alternativePackageName the optional runtime alternative to the
	 * Jaxb package name used at generation time
	 */
	public final void setAlternativePackageName(
			final String alternativePackageName) {
		mCoxbGenContext.setAlternativePackageName(alternativePackageName);
	}

	/**
	 * @return the alternate factory to used rather than the JAXB one.
	 */
	public final String getAlternativeFactoryName() {
		return mCoxbGenContext.getAlternativeFactoryName();
	}

	/**
	 * @param targetFactoryName the alternate factory to used rather than the
	 * JAXB one
	 */
	public final void setAlternativeFactoryName(
			final String targetFactoryName) {
		mCoxbGenContext.setAlternativeFactoryName(targetFactoryName);
	}
	
	/**
	 * @return a list of alternative class names to Jaxb class names
	 */
	public final List < AlternativeClassName > getAlternativeClassNameMap() {
		return mCoxbGenContext.getAlternativeClassNameMap();
	}
	
	/**
	 * @param alternativeClassNameMap a list of alternative class names to Jaxb
	 * class names
	 */
	public final void setAlternativeClassNameMap(
			final List < AlternativeClassName > alternativeClassNameMap) {
		mCoxbGenContext.setAlternativeClassNameMap(alternativeClassNameMap);
	}

	/**
	 * Add a mapping between a Jaxb class name and an alternative class name.
	 * @return an inner class name
	 */
	public final AlternativeClassName createAlternativeClassName() {
		if (getAlternativeClassNameMap() == null) {
			setAlternativeClassNameMap(
					new ArrayList < AlternativeClassName >());
		}
		AlternativeClassName aClassName = new AlternativeClassName();
		getAlternativeClassNameMap().add(aClassName);
		return aClassName;
	}
	

}

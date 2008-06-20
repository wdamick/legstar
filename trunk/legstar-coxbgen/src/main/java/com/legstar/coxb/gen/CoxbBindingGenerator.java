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

import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.host.HostException;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

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
	private CoxbGenModel mCoxbGenModel = new CoxbGenModel();
	
	/** List of jaxb root class names for which we need to generate
	 * binding classes. Beware: this is not reflected in the model. */
	private List < JaxbRootClass > mJaxbRootClasses;

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
    	if (getJaxbBinDir() == null ||	getJaxbBinDir().length() == 0) {
    		jaxbObjectFactory = getObjectFactory(getJaxbPackageName());
    	} else {
    	/* Create an instance of the JAXB object factory */
    		jaxbObjectFactory = getObjectFactory(
    				getJaxbPackageName(), getJaxbBinDir());
    	}
    	
    	for (String jaxbRootClassName : getJaxbRootClassNames()) {
    	
	    	/* Create an instance of the JAXB root object */
	    	Object jaxbRootObject = getRootObject(
	                jaxbObjectFactory, jaxbRootClassName);
	    	
	    	try {
	        	/* Create a visitor */
	        	CoxbGenReflectVisitor visitor =
	        		new CoxbGenReflectVisitor(mCoxbGenModel);
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
 
    }
    
	/**
	 * Checks that properties set are valid.
	 */
	private void checkInput() {
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("checkInput started");
			mCoxbGenModel.traceContext();
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
    	
    	/* There must be at least one jaxb root class name to process */
    	if (getJaxbRootClassNames() == null
    			|| getJaxbRootClassNames().size() == 0) {
			throw (new BuildException(
                    "You must specify at least one JAXB root class name"));
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
			String createName = "create" + rootObjectName;
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
	 * @deprecated
	 * Use <code>getJaxbRootClassName</code> instead
	 * @return Returns the JAXB root object name.
	 */
	public final String getJaxbRootObjectName() {
		return getJaxbRootClassName();
	}

	/**
	 * @deprecated
	 * Use <code>setJaxbRootClassName</code> instead
	 * @param objectName The JAXB root object name to set.
	 */
	public final void setJaxbRootObjectName(
			final String objectName) {
		setJaxbRootClassName(objectName);
	}

	/**
	 * This is an alternative to jaxbRootClassNames used when a
	 * single class is to be bound.
	 * @return Returns the JAXB root class name.
	 */
	public final String getJaxbRootClassName() {
		if (mCoxbGenModel.getJaxbRootClassNames() != null
				&& mCoxbGenModel.getJaxbRootClassNames().size() > 0) {
			return mCoxbGenModel.getJaxbRootClassNames().get(0);
		}
		return null;
	}

	/**
	 * @param objectName The JAXB root class name to set.
	 */
	public final void setJaxbRootClassName(
			final String objectName) {
		addJaxbRootClass(objectName);
	}

	/**
	 * @return the XML schema file
	 */
	public final File getXsdFile() {
		return mCoxbGenModel.getXsdFile();
	}

	/**
	 * @param xsdFile the XML schema file to set
	 */
	public final void setXsdFile(
			final File xsdFile) {
		mCoxbGenModel.setXsdFile(xsdFile);
	}

	/**
	 * @return the current target directory
	 */
	public final File getTargetDir() {
		return mCoxbGenModel.getCoxbSrcDir();
	}

	/**
	 * @param targetDir the target directory to set
	 */
	public final void setTargetDir(final File targetDir) {
		mCoxbGenModel.setCoxbSrcDir(targetDir);
	}

	/**
	 * @deprecated
	 * Use <code>getJaxbBinDir</code> instead
	 * @return the location where JAXB classes live
	 */
	public final File getJaxbDir() {
		return mCoxbGenModel.getJaxbBinDir();
	}

	/**
	 * @deprecated
	 * Use <code>setJaxbBinDir</code> instead
	 * @param jaxbDir the JAXB location to set
	 */
	public final void setJaxbDir(final File jaxbDir) {
		mCoxbGenModel.setJaxbBinDir(jaxbDir);
	}

	/**
	 * @return the location where JAXB classes live
	 */
	public final File getJaxbBinDir() {
		return mCoxbGenModel.getJaxbBinDir();
	}

	/**
	 * @param jaxbBinDir the JAXB location to set
	 */
	public final void setJaxbBinDir(final File jaxbBinDir) {
		mCoxbGenModel.setJaxbBinDir(jaxbBinDir);
	}

	/**
	 * @return the package name used for JAXB classes
	 */
	public final String getJaxbPackageName() {
		return mCoxbGenModel.getJaxbPackageName();
	}

	/**
	 * @param jaxbPackageName the JAXB classes package name to set
	 */
	public final void setJaxbPackageName(final String jaxbPackageName) {
		mCoxbGenModel.setJaxbPackageName(jaxbPackageName);
	}

	/**
	 * @return the package name for generated binding classes
	 */
	public final String getCoxbPackageName() {
		return mCoxbGenModel.getCoxbPackageName();
	}

	/**
	 * @param coxbPackageName package name for generated binding classes to set
	 */
	public final void setCoxbPackageName(final String coxbPackageName) {
		mCoxbGenModel.setCoxbPackageName(coxbPackageName);
	}

	/**
	 * @return the optional runtime alternative to the Jaxb package name used at
	 * generation time
	 */
	public final String getAlternativePackageName() {
		return mCoxbGenModel.getAlternativePackageName();
	}

	/**
	 * @param alternativePackageName the optional runtime alternative to the
	 * Jaxb package name used at generation time
	 */
	public final void setAlternativePackageName(
			final String alternativePackageName) {
		mCoxbGenModel.setAlternativePackageName(alternativePackageName);
	}

	/**
	 * @return the alternate factory to used rather than the JAXB one.
	 */
	public final String getAlternativeFactoryName() {
		return mCoxbGenModel.getAlternativeFactoryName();
	}

	/**
	 * @param targetFactoryName the alternate factory to used rather than the
	 * JAXB one
	 */
	public final void setAlternativeFactoryName(
			final String targetFactoryName) {
		mCoxbGenModel.setAlternativeFactoryName(targetFactoryName);
	}
	
	/**
	 * Add a JAXB root class name holder object.
	 * @param className the class name to add
	 */
	public final void addJaxbRootClass(final String className) {
		JaxbRootClass jaxbRootClass = createJaxbRootClass();
		jaxbRootClass.setName(className);
		mCoxbGenModel.addJaxbRootClassName(className);
	}

	/**
	 * Creates a JAXB root class name holder object.
	 * @return a JAXB root class name holder object
	 */
	public final JaxbRootClass createJaxbRootClass() {
		if (mJaxbRootClasses == null) {
			mJaxbRootClasses = new ArrayList < JaxbRootClass >();
		}
		JaxbRootClass jaxbRootClass = new JaxbRootClass();
		mJaxbRootClasses.add(jaxbRootClass);
		return jaxbRootClass;
	}
	
	/**
	 * @return the list of jaxb root class names to process
	 */
	public final List < String > getJaxbRootClassNames() {
		List < String > classNames = new ArrayList < String >();
		if (mJaxbRootClasses != null) {
			for (JaxbRootClass className : mJaxbRootClasses) {
				classNames.add(className.getName());
			}
		}
		return classNames;
	}
	
	/**
	 * Represent a simple inner element for the ant task. This element 
	 * holds a jaxb root class name. These elements are useful when there
	 * are more than one jaxb class name to process.
	 */
	public class JaxbRootClass {
		
		/** Name of the inner class. */
		private String mName;
		
		/** Needs to be a public constructor. */
		public JaxbRootClass() { }
		
		/**
		 * @param name the class name
		 */
		public final void setName(final String name) {
			mName = name;
		}
		
		/**
		 * @return the current text value
		 */
		public final String getName() {
			return mName;
		}
	}

}

package com.legstar.j2sc.gen;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.SchemaOutputResolver;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.namespace.QName;
import javax.xml.transform.Result;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

import com.legstar.xsdc.gen.XsdCobolAnnotator;
import com.sun.xml.bind.api.JAXBRIContext;
import com.sun.xml.bind.api.impl.NameConverter;

/**
 * This ant task generates an XML schema with COBOL annotation from one or
 * more Java classes taken from the same package.
 *
 */
public class JavaToXsdCobolTask extends Task {

	/** Logger. */
	private static final Log LOG =
		LogFactory.getLog(JavaToXsdCobolTask.class);

	/** Package name of input classes. */
	private String mPackageName;

	/** Package name of target JAXB classes as it appears in the generated
	 *  XSD annotations. */
	private String mJaxbPackageName;

	/** The target schema namespace. */
	private String mNamespace = "";
	
	/** List of input classes. Only top level classes are needed since
	 * schemagen will reflect on these classes and recursively process
	 * children. */
	private List < RootClass > mRootClassNames;

	/** The target directory where annotated XSD will be created. */
	private File mTargetDir;

	/** The target annotated XSD file name. */
	private String mTargetXsdFileName;
	
	/** When extra XSD elements are created from a type name, this suffix 
	 * will be added to form a unique name. */
	private String mElementSuffix = "";

	/** The corresponding list of java classes. */
	private List < Class < ? > > mClassTypes;

	/**
	 *  The ant execute method. Generates a new annotated schema.
	 */
	public final void execute() {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Java to Cobol annotated XML Schema generator started");
		}
		checkInput();
		try {
			generate();
		} catch (IOException e) {
			throw (new BuildException(e));
		} catch (JAXBException e) {
			throw (new BuildException(e));
		}
	}

	/**
	 * Checks that properties set are valid.
	 */
	private void checkInput() {

		if (LOG.isDebugEnabled()) {
			LOG.debug("checkInput started");
			LOG.debug("   Input package name       = " + mPackageName);
			LOG.debug("   Target Jaxb Package name = " + mJaxbPackageName);
			LOG.debug("   Target namespace name    = " + mNamespace);
			if (mRootClassNames != null) {
				for (RootClass className : mRootClassNames) {
					LOG.debug("   Class name           = "
							+ className.getName());
				}
			}
			LOG.debug("   Elements name suffix     = " + mElementSuffix);
			LOG.debug("   Target directory         = " + mTargetDir);
			LOG.debug("   Target Xsd file name     = " + mTargetXsdFileName);
		}
		/* Check that we have a valid target directory.  */
		if (mTargetDir == null) {
			throw (new BuildException(
			"You must provide a target directory"));
		}
		if (!mTargetDir.exists()) {
			throw (new BuildException(
					"Directory " + mTargetDir + " does not exist"));
		}
		if (!mTargetDir.isDirectory() || !mTargetDir.canWrite()) {
			throw (new BuildException(
					mTargetDir + " is not a directory or is not writable"));
		}

		/* Set a valid target annotated XSD file name */
		if (mTargetXsdFileName == null || mTargetXsdFileName.length() == 0) {
			throw (new BuildException(
			"You must provide a target xsd file name"));
		}
		/* There must be at least one class name. */
		if (mRootClassNames == null || mRootClassNames.size() == 0) {
			throw (new BuildException(
			"You must provide at least one class name"));
		}

		/* Class names should not be qualified. */
		for (RootClass className : mRootClassNames) {
			if (className == null || className.getName() == null) {
				throw (new BuildException(
						"Class name has null value"));
			}
			if (className.getName().indexOf('.') != -1) {
				throw (new BuildException(
					"Class name " + className.getName()
					+ " should not be qualified"));
			}
		}

		/* Classes must be available from the classpath. */
		try {
			mClassTypes = new ArrayList < Class < ? > >();
			for (RootClass className : mRootClassNames) {
				String qualClassName = className.getName();
				if (mPackageName != null && mPackageName.length() > 0) {
					qualClassName = mPackageName + '.' + qualClassName;
				}
				mClassTypes.add(Class.forName(qualClassName));
			}
		} catch (ClassNotFoundException e) {
			throw (new BuildException(e));
		}

		if (LOG.isDebugEnabled()) {
			LOG.debug("checkInput ended");
		}
	}
	
	/**
	 * The process has 2 steps:
	 * - Step1 is sun's schemagen ant task. It creates an Xml Schema without any
	 *   annotations.
	 * - Step2 is XsdCobolAnnotator which actually generates the annotated
	 *   schema
	 * @throws IOException if file operation fails
	 * @throws JAXBException if JAXB operation fails
	 */
	private void generate() throws IOException, JAXBException {
        /* The schema generated by Sun's schemagen is temporary. */
		File tempXsdFile = File.createTempFile("schema", "xsd");

        Map < String, Object > properties = new HashMap < String, Object >();
        if (getNamespace() != null && getNamespace().length() > 0) {
            properties.put(
            		JAXBRIContext.DEFAULT_NAMESPACE_REMAP, getNamespace());
        }
        
        Class < ? >[] classes = new Class[mClassTypes.size()];
        int i = 0;
        for (Class < ? >  clazz : mClassTypes) {
        	classes[i] = clazz;
        	i++;
        }
        JAXBContext context = JAXBContext.newInstance(classes, properties);
        context.generateSchema(new MySchemaOutputResolver(tempXsdFile));

        /* Now annotate the temporary schema to create the final one */
        XsdCobolAnnotator task = new XsdCobolAnnotator();
        task.setInputXsdUri(tempXsdFile.toURI());
        task.setJaxbPackageName(mJaxbPackageName);
        task.setTargetDir(mTargetDir);
        task.setTargetXsdFileName(mTargetXsdFileName);
        task.setRootElements(getRootElements());
        task.execute();
	}
	
    /**
     * Implementation of JAXB output resolver.
     */
    public class MySchemaOutputResolver extends SchemaOutputResolver {
        /** The schema file. */
    	private File mSchemaFile;
        
        /**
         * Constructor for a schema file.
         * @param schemaFile the schema file
         */
        public MySchemaOutputResolver(final File schemaFile) {
        	mSchemaFile = schemaFile;
        }
        /** {@inheritDoc} */
        public final Result createOutput(
        		final String namespaceUri,
        		final String suggestedFileName) throws IOException {
            return new StreamResult(mSchemaFile);
        }
    }
    
    /**
     * The XML schema produced by sun's schemagen does not necessarily
     * contain root elements.
     * Root elements are created by schemagen when java classes are annotated
     * with <code>XmlRootElement</code>. Otherwise, there are only complexTypes.
     * Since LegStar needs root elements in XML Schemas, this will determine
     * additional root elements if none are found by XsdCobolAnnotator.
     * @return a map of root elements
     */
    private Map < QName, QName > getRootElements() {
        Map < QName, QName > rootElements = new HashMap < QName, QName >();
        for (Class < ? > clazz : mClassTypes) {
        	/* If this class was annotated as root element, this means the
        	 * developer is explicitly specifying root elements. Therefore
        	 * we should not attempt to create implicit ones at all. */
        	XmlRootElement xmlRootElement = clazz.getAnnotation(
        			XmlRootElement.class);
        	if (xmlRootElement != null) {
        		return new HashMap < QName, QName >();
        	}
        	
        	XmlType xmlType = clazz.getAnnotation(XmlType.class);
        	if (xmlType == null) {
            	/* Since the developer did not specify an XmlType, the standard
            	 * jaxb naming will apply and the default namespace will be
            	 * used. */
        		String jaxbName = NameConverter.standard.toVariableName(
        				clazz.getSimpleName());
                rootElements.put(new QName(mNamespace, jaxbName),
                        new QName(mNamespace, jaxbName + mElementSuffix));
        	} else {
        		/* If XmlType does not explicitly provide a complex type name
        		 * use the default jaxb naming. */
        		String typeName = xmlType.name();
        		if (typeName .equals("##default")) {
        			typeName = NameConverter.standard.toVariableName(
            				clazz.getSimpleName());
        		}
        		String namespace = xmlType.namespace();
        		if (namespace.equals("##default")) {
            		namespace = mNamespace;
        		}
                rootElements.put(
                		new QName(namespace, typeName),
                        new QName(namespace,
                        		typeName + mElementSuffix));
        	}
        }
        return rootElements;
     }


	/**
	 * @return the package name of input classes
	 */
	public final String getPackageName() {
		return mPackageName;
	}

	/**
	 * @param packageName the package name of input classes to set
	 */
	public final void setPackageName(final String packageName) {
		mPackageName = packageName;
	}

	/**
	 * List of input classes. Only top level classes are needed since
	 * schemagen will reflect on these classes and recursively process
	 * children.
	 * @return List of input classes
	 */
	public final List < String > getClassNames() {
		List < String > classNames = new ArrayList < String >();
		for (RootClass className : mRootClassNames) {
			classNames.add(className.getName());
		}
		return classNames;
	}
	
	/**
	 * Add a top level class name.
	 * @param className the class name to add
	 */
	public final void addRootClass(final String className) {
		RootClass rootClass = createRootClass();
		rootClass.setName(className);
	}

	/**
	 * Only top level classes are needed since
	 * schemagen will reflect on these classes and recursively process
	 * children.
	 * @param classNames the list of input classes to set
	 */
	public final void setClassNames(final List < String > classNames) {
		for (String className : classNames) {
			addRootClass(className);
		}
	}

	/**
	 * @return the current target directory
	 */
	public final File getTargetDir() {
		return mTargetDir;
	}

	/**
	 * @param targetDir the target directory to set
	 */
	public final void setTargetDir(final File targetDir) {
		mTargetDir = targetDir;
	}

	/**
	 * @return the target annotated XSD file name
	 */
	public final String getTargetXsdFileName() {
		return mTargetXsdFileName;
	}

	/**
	 * @param targetXsdFileName the target annotated XSD file name to set
	 */
	public final void setTargetXsdFileName(final String targetXsdFileName) {
		mTargetXsdFileName = targetXsdFileName;
	}

	/**
	 * Add a top level class name without package name.
	 * @return an inner class name
	 */
	public final RootClass createRootClass() {
		if (mRootClassNames == null) {
			mRootClassNames = new ArrayList < RootClass >();
		}
		RootClass className = new RootClass();
		mRootClassNames.add(className);
		return className;
	}
	
	/**
	 * Represent a simple inner element for the ant task.
	 */
	public class RootClass {
		
		/** Name of the inner class. */
		private String mName;
		
		/** Needs to be a public constructor. */
		public RootClass() { }
		
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

	/**
	 * @return the target schema namespace
	 */
	public final String getNamespace() {
		return mNamespace;
	}

	/**
	 * @param namespace the target schema namespace to set
	 */
	public final void setNamespace(final String namespace) {
		mNamespace = namespace;
	}

	/**
	 * @return when extra XSD elements are created from a type name,
	 * this suffix will be added to form a unique name
	 */
	public final String getElementSuffix() {
		return mElementSuffix;
	}

	/**
	 * @param elementSuffix when extra XSD elements are created from a type
	 * name, this suffix will be added to form a unique name to set
	 */
	public final void setElementSuffix(final String elementSuffix) {
		mElementSuffix = elementSuffix;
	}

	/**
	 * Package name of target JAXB classes as it appears in the generated
	 *  XSD annotations.
	 * @return the mJaxbPackageName JAXB package name
	 */
	public final String getJaxbPackageName() {
		return mJaxbPackageName;
	}

	/**
	 * Package name of target JAXB classes as it appears in the generated
	 *  XSD annotations.
	 * @param jaxbPackageName the JAXB package name to set
	 */
	public final void setJaxbPackageName(final String jaxbPackageName) {
		mJaxbPackageName = jaxbPackageName;
	}
}

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
package com.legstar.coxb.gen;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

import com.legstar.codegen.CodeGenUtil;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;

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

    /** Container for all parameters to move around. */
    private CoxbGenModel _coxbGenModel = new CoxbGenModel();

    /** Folder where generated files are created. */
    private File _outputFolder;

    /**
     * List of jaxb root class names for which we need to generate
     * binding classes. Beware: this is not reflected in the model.
     */
    private List < JaxbRootClass > _jaxbRootClasses;

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * The ant method. Generates COXB binding code.
     */
    public void execute() {
        checkInput();

        /*
         * If we are not provided with a physical location for JAXB classes,
         * assume they are available from the current classpath.
         */
        Object jaxbObjectFactory;
        if (getJaxbBinDir() == null || getJaxbBinDir().length() == 0) {
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
                        new CoxbGenReflectVisitor(_coxbGenModel,
                                _outputFolder);
                /* Bind the root object to a COXB type */
                CComplexReflectBinding ce = new CComplexReflectBinding(
                        jaxbObjectFactory, jaxbRootObject);
                /* Visit COXB type and all subtypes recursively */
                ce.accept(visitor);

                /* For root objects, generate transformer classes */
                visitor.getWriter().writeHostToJavaTransformer(ce);
                visitor.getWriter().writeJavaToHostTransformer(ce);
                visitor.getWriter().writeTransformers(ce);
                if (isXmlTransformers()) {
                    visitor.getWriter().writeHostToXmlTransformer(ce);
                    visitor.getWriter().writeXmlToHostTransformer(ce);
                    visitor.getWriter().writeXmlTransformers(ce);
                }

            } catch (HostException e) {
                _log.error(CoxbGenWriter.BINDING_GENERATOR_NAME + " failure ",
                        e);
                throw (new BuildException(
                        "HostException " + e.getMessage()));
            } catch (CoxbGenException e) {
                _log.error(CoxbGenWriter.BINDING_GENERATOR_NAME + " failure ",
                        e);
                throw new BuildException(e);
            }
        }

    }

    /**
     * Checks that properties set are valid.
     */
    protected void checkInput() {

        if (_log.isDebugEnabled()) {
            _log.debug("checkInput started");
            _log.debug("coxbGenModel: " + _coxbGenModel.toString());
        }

        /*
         * If user did not provide a JAXB package name, we need to get it
         * from the XML schema annotations.
         */
        if (getJaxbPackageName() == null
                || getJaxbPackageName().length() == 0) {
            throw (new BuildException(
                        "You must specify either a JAXB package name or"
                                + " an XML schema file name"));
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

        /* Check that we have a valid output folder */
        try {
            _outputFolder = createOutputFolder(_coxbGenModel);
        } catch (CoxbGenException e) {
            throw (new BuildException(e));
        }

    }

    /**
     * Create the folder which will receive the generated classes.
     * <p/>
     * Folder includes the COXB package path if any. When the COXB package path
     * is provided, the output is cleaned from any previous generation
     * artifacts.
     * 
     * @param coxbGenModel the generation model
     * 
     * @return the generation folder
     * @throws CoxbGenException if generation folder cannot be determined
     */
    public static File createOutputFolder(final CoxbGenModel coxbGenModel)
            throws CoxbGenException {
        try {
            File outputFolder = coxbGenModel.getCoxbSrcDir();
            if (coxbGenModel.getCoxbPackageName() != null) {
                outputFolder = new File(coxbGenModel.getCoxbSrcDir(),
                        CodeGenUtil.relativeLocation(
                                coxbGenModel.getCoxbPackageName()));
            }
            CodeGenUtil.checkDirectory(outputFolder, true);
            if (coxbGenModel.getCoxbPackageName() != null) {
                FileUtils.cleanDirectory(outputFolder);
            }
            return outputFolder;
        } catch (IOException e) {
            throw new CoxbGenException(e);
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

            /* Create a new class loader with the directory */
            ClassLoader cl = getURLClassLoader(jaxbDir.toURI().toURL());

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
     * Create a class loader from a URL.
     * 
     * @param url where classes are located
     * @return the class loader
     */
    @SuppressWarnings("unchecked")
    private static ClassLoader getURLClassLoader(final URL url) {
        return (ClassLoader) AccessController
                .doPrivileged(new PrivilegedAction() {
                    public Object run() {
                        ClassLoader cl = null;
                        try {
                            URL[] urls = new URL[] { url };
                            cl = new java.net.URLClassLoader(urls);
                            return cl;
                        } catch (SecurityException e) {
                            throw new BuildException(
                                    "SecurityException " + e.getMessage());
                        }
                    }
                });
    }

    /**
     * Loads the object factory class using the current class loader assuming
     * the JAXB classes are available in the current classpath and returns a
     * new instance of it.
     * 
     * @param packageName the package containing a JAXB Object Factory
     * @return a JAXB Object factory
     */
    protected Object getObjectFactory(
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
     * Returns a new instance of the requested JAXB object.
     * 
     * @param jaxbObjectFactory an instance of a JAXB Object Factory
     * @param rootObjectName the JAXB root object name (non qualified)
     * @return an instance of the JAXB root object
     */
    protected Object getRootObject(
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
     * This is an alternative to jaxbRootClassNames used when a
     * single class is to be bound.
     * 
     * @return Returns the JAXB root class name.
     */
    public String getJaxbRootClassName() {
        if (_coxbGenModel.getJaxbRootClassNames() != null
                && _coxbGenModel.getJaxbRootClassNames().size() > 0) {
            return _coxbGenModel.getJaxbRootClassNames().get(0);
        }
        return null;
    }

    /**
     * @param objectName The JAXB root class name to set.
     */
    public void setJaxbRootClassName(
            final String objectName) {
        addJaxbRootClass(objectName);
    }

    /**
     * @return the XML schema file
     */
    public File getXsdFile() {
        return _coxbGenModel.getXsdFile();
    }

    /**
     * @param xsdFile the XML schema file to set
     */
    public void setXsdFile(
            final File xsdFile) {
        _coxbGenModel.setXsdFile(xsdFile);
    }

    /**
     * @return the current target directory
     */
    public File getTargetDir() {
        return _coxbGenModel.getCoxbSrcDir();
    }

    /**
     * @param targetDir the target directory to set
     */
    public void setTargetDir(final File targetDir) {
        _coxbGenModel.setCoxbSrcDir(targetDir);
    }

    /**
     * @deprecated
     *             Use <code>getJaxbBinDir</code> instead
     * @return the location where JAXB classes live
     */
    public File getJaxbDir() {
        return _coxbGenModel.getJaxbBinDir();
    }

    /**
     * @deprecated
     *             Use <code>setJaxbBinDir</code> instead
     * @param jaxbDir the JAXB location to set
     */
    public void setJaxbDir(final File jaxbDir) {
        _coxbGenModel.setJaxbBinDir(jaxbDir);
    }

    /**
     * @return the location where JAXB classes live
     */
    public File getJaxbBinDir() {
        return _coxbGenModel.getJaxbBinDir();
    }

    /**
     * @param jaxbBinDir the JAXB location to set
     */
    public void setJaxbBinDir(final File jaxbBinDir) {
        _coxbGenModel.setJaxbBinDir(jaxbBinDir);
    }

    /**
     * @return the package name used for JAXB classes
     */
    public String getJaxbPackageName() {
        try {
            return _coxbGenModel.getJaxbPackageName();
        } catch (CoxbGenException e) {
            throw (new BuildException(
                    "JAXB package name was not provided"
                            + " and could not be recovered from XML schema file",
                    e));
        }
    }

    /**
     * @param jaxbPackageName the JAXB classes package name to set
     */
    public void setJaxbPackageName(final String jaxbPackageName) {
        _coxbGenModel.setJaxbPackageName(jaxbPackageName);
    }

    /**
     * @return the package name for generated binding classes
     */
    public String getCoxbPackageName() {
        try {
            return _coxbGenModel.getCoxbPackageName();
        } catch (CoxbGenException e) {
            throw (new BuildException(
                    "COXB package name was not provided"
                            + " and could not be recovered from XML schema file",
                    e));
        }
    }

    /**
     * @param coxbPackageName package name for generated binding classes to set
     */
    public void setCoxbPackageName(final String coxbPackageName) {
        _coxbGenModel.setCoxbPackageName(coxbPackageName);
    }

    /**
     * @return true if Host to XML transformers generation is turned on
     */
    public boolean isXmlTransformers() {
        return _coxbGenModel.isXmlTransformers();
    }

    /**
     * @param xmlTransformers true if Host to XML transformers generation is
     *            turned on
     */
    public void setXmlTransformers(final boolean xmlTransformers) {
        _coxbGenModel.setXmlTransformers(xmlTransformers);
    }

    /**
     * @return the optional runtime alternative to the Jaxb package name used at
     *         generation time
     */
    public String getAlternativePackageName() {
        return _coxbGenModel.getAlternativePackageName();
    }

    /**
     * @param alternativePackageName the optional runtime alternative to the
     *            Jaxb package name used at generation time
     */
    public void setAlternativePackageName(
            final String alternativePackageName) {
        _coxbGenModel.setAlternativePackageName(alternativePackageName);
    }

    /**
     * @return the alternate factory to used rather than the JAXB one.
     */
    public String getAlternativeFactoryName() {
        return _coxbGenModel.getAlternativeFactoryName();
    }

    /**
     * @param targetFactoryName the alternate factory to used rather than the
     *            JAXB one
     */
    public void setAlternativeFactoryName(
            final String targetFactoryName) {
        _coxbGenModel.setAlternativeFactoryName(targetFactoryName);
    }

    /**
     * Add a JAXB root class name holder object.
     * 
     * @param className the class name to add
     */
    public void addJaxbRootClass(final String className) {
        JaxbRootClass jaxbRootClass = createJaxbRootClass();
        jaxbRootClass.setName(className);
        _coxbGenModel.addJaxbRootClassName(className);
    }

    /**
     * Creates a JAXB root class name holder object.
     * 
     * @return a JAXB root class name holder object
     */
    public JaxbRootClass createJaxbRootClass() {
        if (_jaxbRootClasses == null) {
            _jaxbRootClasses = new ArrayList < JaxbRootClass >();
        }
        JaxbRootClass jaxbRootClass = new JaxbRootClass();
        _jaxbRootClasses.add(jaxbRootClass);
        return jaxbRootClass;
    }

    /**
     * @return the list of jaxb root class names to process
     */
    public List < String > getJaxbRootClassNames() {
        List < String > classNames = new ArrayList < String >();
        if (_jaxbRootClasses != null) {
            for (JaxbRootClass className : _jaxbRootClasses) {
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
    public static class JaxbRootClass {

        /** Name of the inner class. */
        private String mName;

        /** Needs to be a public constructor. */
        public JaxbRootClass() {
        }

        /**
         * @param name the class name
         */
        public void setName(final String name) {
            mName = name;
        }

        /**
         * @return the current text value
         */
        public String getName() {
            return mName;
        }
    }

}

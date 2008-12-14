/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.j2sc.gen;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.SchemaOutputResolver;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.namespace.QName;
import javax.xml.transform.Result;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.BuildException;
import org.xml.sax.SAXParseException;

import com.legstar.codegen.tasks.SourceToXsdCobolTask;
import com.legstar.util.JaxbUtil;
import com.legstar.util.NameUtil;
import com.legstar.xsdc.gen.XsdCobolAnnotator;
import com.sun.istack.NotNull;
import com.sun.xml.bind.api.CompositeStructure;
import com.sun.xml.bind.api.ErrorListener;
import com.sun.xml.bind.api.JAXBRIContext;
import com.sun.xml.bind.v2.model.annotation.RuntimeInlineAnnotationReader;
import com.sun.xml.bind.v2.model.core.ClassInfo;
import com.sun.xml.bind.v2.model.core.Ref;
import com.sun.xml.bind.v2.model.core.TypeInfoSet;
import com.sun.xml.bind.v2.model.impl.ModelBuilder;
import com.sun.xml.bind.v2.model.nav.Navigator;
import com.sun.xml.bind.v2.runtime.IllegalAnnotationsException;
import com.sun.xml.bind.v2.schemagen.XmlSchemaGenerator;

/**
 * This ant task generates an XML schema with COBOL annotation from one or
 * more Java classes taken from a single package.
 *
 */
public class JavaToXsdCobolTask extends SourceToXsdCobolTask {

    /** Logger. */
    private static final Log LOG =
        LogFactory.getLog(JavaToXsdCobolTask.class);

    /** List of input classes. Only top level classes are needed since
     * schemagen will reflect on these classes and recursively process
     * children. */
    private List < RootClass > mRootClassNames;

    /** When extra XSD elements are created from a type name, this suffix 
     * will be added to form a unique name. */
    private String mElementSuffix = "";

    /** The corresponding list of java classes. */
    private List < Class < ? > > mClassTypes;

    /** No arg constructor. */
    public JavaToXsdCobolTask() {
        setModel(new JavaToXsdCobolModel());
    }

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
        } catch (JavaToXsdCobolException e) {
            throw (new BuildException(e));
        }
    }

    /**
     * Checks that properties set are valid.
     */
    private void checkInput() {

        /* Both xsd file name and namespace are mandatory */
        super.checkInput(true, true);

        if (LOG.isDebugEnabled()) {
            if (mRootClassNames != null) {
                for (RootClass className : mRootClassNames) {
                    LOG.debug("   Class name           = "
                            + className.getName());
                }
            }
            LOG.debug("   Elements name suffix     = " + mElementSuffix);
        }

        /* There must be at least one class name. */
        if (mRootClassNames == null || mRootClassNames.size() == 0) {
            throw (new BuildException(
            "You must provide at least one class name"));
        }

        /* Class names should be valid. */
        for (RootClass className : mRootClassNames) {
            if (className == null || className.getName() == null) {
                throw (new BuildException(
                "Class name has null value"));
            }
        }

        /* Classes must be available from the classpath. */
        try {
            mClassTypes = new ArrayList < Class < ? > >();
            for (RootClass className : mRootClassNames) {
                String qualClassName = className.getName();
                mClassTypes.add(JaxbUtil.loadClass(qualClassName));
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
     * @throws JavaToXsdCobolException if file or JAXB operation fails
     */
    private void generate() throws JavaToXsdCobolException {
        try {
            /* The schema generated by Sun's schemagen is temporary. */
            File tempXsdFile = File.createTempFile("schema", "xsd");
            Map < String, String > complexTypeToJavaClassMap =
                new HashMap < String, String >();

            Map < String, Object > properties =
                new HashMap < String, Object >();
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
            generateSchema(classes, tempXsdFile, complexTypeToJavaClassMap);

            /* Now annotate the temporary schema to create the final one */
            XsdCobolAnnotator task = new XsdCobolAnnotator();
            task.setInputXsdUri(tempXsdFile.toURI());
            task.setJaxbPackageName(getJaxbPackageName());
            task.setJaxbTypeClassesSuffix(getJaxbTypeClassesSuffix());
            task.setTargetDir(getTargetDir());
            task.setTargetXsdFileName(getTargetXsdFileName());
            task.setRootElements(getRootElements());
            task.setComplexTypeToJavaClassMap(complexTypeToJavaClassMap);
            task.execute();
        } catch (IOException e) {
            throw new JavaToXsdCobolException(e);
        }
    }

    /**
     * This is basically the same code as
     * <code>JAXBContextImpl.createSchemaGenerator</code>. The reason it is
     * duplicated here is because JAXBContextImpl is declared final and
     * cannot be derived from and all useful methods are private. 
     * What we need is to grab the relationship between Class names and
     * XSD complex type names. JAXB does its own name conversion and the
     * relationship between the original class name and the new complex type
     * name is lost with the standard JAXB schemagen ant Task.
     * @param classes a set of JAXB classes
     * @param tempXsdFile the output XML Schema
     * @param complexTypeToJavaClassMap an output map between complex types
     * and java class names
     * @throws JavaToXsdCobolException is something goes wrong
     */
    @SuppressWarnings("unchecked")
    public final void generateSchema(
            final Class < ? >[] classes,
            final File tempXsdFile,
            final Map < String, String > complexTypeToJavaClassMap)
    throws JavaToXsdCobolException {
        try {
            final SAXParseException[] exceptions = new SAXParseException[1];
            @NotNull final  Map < Class, Class > subclassReplacements =
                Collections.emptyMap();

            final ModelBuilder < Type, Class, Field, Method > builder =
                new ModelBuilder < Type, Class, Field, Method >(
                        new RuntimeInlineAnnotationReader(),
                        Navigator.REFLECTION,
                        subclassReplacements,
                        getNamespace());

            IllegalAnnotationsException.Builder errorHandler =
                new IllegalAnnotationsException.Builder();
            builder.setErrorHandler(errorHandler);

            for (Class < ? > c : classes) {
                if (c == CompositeStructure.class) {
                    // CompositeStructure doesn't have TypeInfo, so skip it.
                    // We'll add JaxBeanInfo for this later automatically
                    continue;
                }
                builder.getTypeInfo(new Ref < Type, Class >(c));
            }

            TypeInfoSet < Type, Class, Field, Method> infoSet = builder.link();

            errorHandler.check();
            assert infoSet != null
            : "if no error was reported, the link must be a success";

            /* Store the name mapping (Java Class to XSD Complex Type) */
            for (ClassInfo < Type, Class > ci : infoSet.beans().values()) {
                complexTypeToJavaClassMap.put(
                        ci.getTypeName().getLocalPart(),
                        ci.getClazz().getName()
                );
            }

            XmlSchemaGenerator < Type, Class, Field, Method > xsdgen =
                new XmlSchemaGenerator < Type, Class, Field, Method>(
                        infoSet.getNavigator(), infoSet);

            xsdgen.write(
                    new MySchemaOutputResolver(tempXsdFile),
                    new ErrorListener() {
                        public void error(final SAXParseException e) {
                            exceptions[0] = e;
                        }

                        public void fatalError(final SAXParseException e) {
                            exceptions[0] = e;
                        }

                        public void warning(final SAXParseException e) { }
                        public void info(final SAXParseException e) { }
                    });
            if (exceptions[0] != null) {
                throw new JavaToXsdCobolException(exceptions[0]);
            }

        } catch (IllegalAnnotationsException e) {
            throw new JavaToXsdCobolException(e);
        } catch (IOException e) {
            throw new JavaToXsdCobolException(e);
        }

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
                String jaxbName = NameUtil.toVariableName(
                        clazz.getSimpleName());
                rootElements.put(new QName(getNamespace(), jaxbName),
                        new QName(getNamespace(), jaxbName + mElementSuffix));
            } else {
                /* If XmlType does not explicitly provide a complex type name
                 * use the default jaxb naming. */
                String typeName = xmlType.name();
                if (typeName .equals("##default")) {
                    typeName = NameUtil.toVariableName(
                            clazz.getSimpleName());
                }
                String namespace = xmlType.namespace();
                if (namespace.equals("##default")) {
                    namespace = getNamespace();
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
}

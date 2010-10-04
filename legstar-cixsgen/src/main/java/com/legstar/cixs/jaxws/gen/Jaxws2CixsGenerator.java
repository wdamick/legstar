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
package com.legstar.cixs.jaxws.gen;

import java.io.File;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.gen.model.CixsStructure;
import com.legstar.cixs.gen.model.options.WebServiceParameters;
import com.legstar.cixs.jaxws.model.AntBuildJaxws2CixsModel;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.CodeGenUtil;
import com.legstar.coxb.host.HostException;

/**
 * This Ant task creates the various Jaxws artifacts needed to implement
 * a Jaxws Web Service that acts as an adapter for a mainframe program.
 * Web service clients can consume this adapter as any other Web Service
 * but internally the adapter use the LegStar transport to call a
 * a mainframe program.
 */
public class Jaxws2CixsGenerator extends AbstractCixsJaxwsGenerator {

    /** This generator name. */
    public static final String JAXWS_TO_CIXS_GENERATOR_NAME =
            "LegStar Mainframe Web Service adapter generator";

    /** Velocity template for service interface. */
    public static final String SERVICE_INTERFACE_VLC_TEMPLATE =
            "vlc/j2c-service-interface.vm";

    /** Velocity template for service implementation. */
    public static final String SERVICE_IMPLEMENTATION_VLC_TEMPLATE =
            "vlc/j2c-service-implementation.vm";

    /** Velocity template for service header. */
    public static final String SERVICE_HEADER_VLC_TEMPLATE =
            "vlc/j2c-service-header.vm";

    /** Velocity template for service ant-build-war. */
    public static final String SERVICE_ANT_BUILD_WAR_VLC_TEMPLATE =
            "vlc/j2c-service-ant-build-war-xml.vm";

    /** Velocity template for service ant-build-jar. */
    public static final String SERVICE_ANT_BUILD_JAR_VLC_TEMPLATE =
            "vlc/j2c-service-ant-build-jar-xml.vm";

    /** Velocity template for service ant-deploy. */
    public static final String SERVICE_ANT_DEPLOY_VLC_TEMPLATE =
            "vlc/j2c-service-ant-deploy-xml.vm";

    /** Velocity template for service sun-jaxws-xml. */
    public static final String SERVICE_SUN_JAXWS_XML_VLC_TEMPLATE =
            "vlc/j2c-service-sun-jaxws-xml.vm";

    /** Velocity template for service web-xml. */
    public static final String SERVICE_WEB_XML_VLC_TEMPLATE =
            "vlc/j2c-service-web-xml.vm";

    /** Velocity template for package html. */
    public static final String SERVICE_PACKAGE_HTML_VLC_TEMPLATE =
            "vlc/j2c-package-html.vm";

    /** Velocity template for fault. */
    public static final String OPERATION_FAULT_VLC_TEMPLATE =
            "vlc/j2c-operation-fault.vm";

    /** Velocity template for fault info. */
    public static final String OPERATION_FAULT_INFO_VLC_TEMPLATE =
            "vlc/j2c-operation-fault-info.vm";

    /** Velocity template for holder. */
    public static final String OPERATION_HOLDER_VLC_TEMPLATE =
            "vlc/j2c-operation-holder.vm";

    /** Velocity template for host program bean. */
    public static final String OPERATION_HOST_PROGRAM_VLC_TEMPLATE =
            "vlc/j2c-operation-host-program.vm";

    /** Velocity template for wrapper. */
    public static final String OPERATION_WRAPPER_VLC_TEMPLATE =
            "vlc/j2c-operation-wrapper.vm";

    /** Velocity template for operation invokers. */
    public static final String OPERATION_PROGRAM_INVOKER_VLC_TEMPLATE =
            "vlc/j2c-operation-program-invoker.vm";

    /** Velocity template for service package-info.java. */
    public static final String SERVICE_PACKAGE_INFO_VLC_TEMPLATE =
            "vlc/j2c-service-package-info.vm";

    /** Velocity template for service ObjectFactory.java. */
    public static final String SERVICE_OBJECTFACTORY_VLC_TEMPLATE =
            "vlc/j2c-service-objectfactory.vm";

    /** The service model name is it appears in templates. */
    private static final String SERVICE_MODEL_NAME = "model";

    /** Will be appended to service name to form a port name. */
    public static final String DEFAULT_WSDL_PORT_NAME_SUFFIX = "Port";

    /**
     * By default the web service name is built from component name and this
     * suffix.
     */
    public static final String DEFAULT_WSDL_SERVICE_NAME_SUFFIX = "Service";

    /**
     * By default the web service namespace is built from component name and
     * this
     * prefix.
     */
    public static final String DEFAULT_WSDL_TARGET_NAMESPACE_PREFIX = "http://cixs.test.legstar.com";

    /**
     * Constructor.
     */
    public Jaxws2CixsGenerator() {
        super(new AntBuildJaxws2CixsModel());
    }

    /** {@inheritDoc} */
    @Override
    public void addExtendedParameters(final Map < String, Object > parameters) {
        /* Contribute the web service parameters */
        getWebServiceParameters().add(parameters);
    }

    /** {@inheritDoc} */
    @Override
    public void checkExtendedExtendedInput() throws CodeGenMakeException {
        try {
            /*
             * Check that we are provided with valid locations to
             * generate in.
             */
            CodeGenUtil.checkDirectory(
                    getTargetSrcDir(), true, "TargetSrcDir");

            /*
             * Check that we are provided with valid locations to
             * reference.
             */
            if (getTargetBinDir() == null) {
                throw (new IllegalArgumentException(
                        "TargetBinDir: No directory name was specified"));
            }
        } catch (IllegalArgumentException e) {
            throw new CodeGenMakeException(e);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void generateExtended(final Map < String, Object > parameters)
            throws CodeGenMakeException {
        /* Determine target files locations */
        File serviceClassFilesDir = CodeGenUtil.classFilesLocation(
                getTargetSrcDir(), getCixsService().getPackageName(), true);
        File serviceWebFilesDir = getTargetWDDDir();
        CodeGenUtil.checkDirectory(serviceWebFilesDir, true);
        File serviceAntFilesDir = getTargetAntDir();
        CodeGenUtil.checkDirectory(serviceAntFilesDir, true);

        /* Produce artifacts */
        generateInterface(
                getCixsJaxwsService(), parameters, serviceClassFilesDir);
        generateImplementation(
                getCixsJaxwsService(), parameters, serviceClassFilesDir);
        generateHeader(
                getCixsJaxwsService(), parameters, serviceClassFilesDir);
        generatePackageInfo(
                getCixsJaxwsService(), parameters, serviceClassFilesDir);
        generateObjectFactory(
                getCixsJaxwsService(), parameters, serviceClassFilesDir);
        generateSunJaxwsXml(
                getCixsJaxwsService(), parameters, serviceWebFilesDir);
        generateWebXml(
                getCixsJaxwsService(), parameters, serviceWebFilesDir);
        generateAntBuildJar(
                getCixsJaxwsService(), parameters, serviceAntFilesDir);
        generateAntBuildWar(
                getCixsJaxwsService(), parameters, serviceAntFilesDir);
        generateAntDeploy(
                getCixsJaxwsService(), parameters, serviceAntFilesDir);

        for (CixsOperation operation : getCixsService().getCixsOperations()) {

            /* Determine target files locations */
            File operationClassFilesDir = CodeGenUtil.classFilesLocation(
                    getTargetSrcDir(), operation.getPackageName(), true);

            generateFault(
                    operation, parameters, operationClassFilesDir);
            generateFaultInfo(
                    operation, parameters, operationClassFilesDir);
            generateWrappers(
                    operation, parameters, operationClassFilesDir);
            generateHolders(
                    operation, parameters, operationClassFilesDir);
            generateProgramInvoker(
                    operation, parameters, operationClassFilesDir);
            generateHostProgram(
                    operation, parameters, operationClassFilesDir);

        }

        generatePackageHtml(
                getCixsJaxwsService(), parameters, serviceClassFilesDir);
    }

    /**
     * Generate default values where they are missing in the model. This
     * will reduce the amount of code in the velocity templates.
     * <p/>
     * The adapter target namespace is used for wrapper JAXB classes.
     */
    protected void completeModel() {
        completeWebServiceParameters();
        getCixsService().setNamespace(
                getWebServiceParameters().getWsdlTargetNamespace());
        for (CixsOperation operation : getCixsService().getCixsOperations()) {
            if (operation.getPackageName() == null
                    || operation.getPackageName().length() == 0) {
                operation.setPackageName(
                        getCixsJaxwsService().getPackageName());
            }
            if (operation.getNamespace() == null
                    || operation.getNamespace().length() == 0) {
                operation.setNamespace(getCixsService().getNamespace());
            }
        }
    }

    /**
     * Provide default values to expose this adapter as a Web Service.
     */
    protected void completeWebServiceParameters() {
        if (getWebServiceParameters().getWsdlServiceName() == null
                || getWebServiceParameters().getWsdlServiceName().length() == 0) {
            getWebServiceParameters().setWsdlServiceName(
                    getCixsService().getName()
                            + DEFAULT_WSDL_SERVICE_NAME_SUFFIX);
        }
        if (getWebServiceParameters().getWsdlPortName() == null
                || getWebServiceParameters().getWsdlPortName().length() == 0) {
            getWebServiceParameters().setWsdlPortName(
                    getCixsService().getName() + DEFAULT_WSDL_PORT_NAME_SUFFIX);
        }
        if (getWebServiceParameters().getWsdlTargetNamespace() == null
                || getWebServiceParameters().getWsdlTargetNamespace().length() == 0) {
            getWebServiceParameters().setWsdlTargetNamespace(
                    DEFAULT_WSDL_TARGET_NAMESPACE_PREFIX
                            + '/' + getCixsService().getName());
        }

    }

    /**
     * Create the Jaxws Interface class file.
     * 
     * @param service the Jaxws service description
     * @param parameters miscellaneous help parameters
     * @param serviceClassFilesDir where to store the generated file
     * @throws CodeGenMakeException if generation fails
     */
    public static void generateInterface(
            final CixsJaxwsService service,
            final Map < String, Object > parameters,
            final File serviceClassFilesDir)
            throws CodeGenMakeException {
        generateFile(JAXWS_TO_CIXS_GENERATOR_NAME,
                SERVICE_INTERFACE_VLC_TEMPLATE,
                SERVICE_MODEL_NAME,
                service,
                parameters,
                serviceClassFilesDir,
                service.getInterfaceClassName() + ".java");
    }

    /**
     * Create a package level doc.
     * 
     * @param service the Jaxws service description
     * @param parameters miscellaneous help parameters
     * @param serviceClassFilesDir where to store the generated file
     * @throws CodeGenMakeException if generation fails
     */
    public static void generatePackageHtml(
            final CixsJaxwsService service,
            final Map < String, Object > parameters,
            final File serviceClassFilesDir)
            throws CodeGenMakeException {
        generateFile(JAXWS_TO_CIXS_GENERATOR_NAME,
                SERVICE_PACKAGE_HTML_VLC_TEMPLATE,
                SERVICE_MODEL_NAME,
                service,
                parameters,
                serviceClassFilesDir,
                "package.html");
    }

    /**
     * Create the Jaxws Implementation class file.
     * 
     * @param service the Jaxws service description
     * @param parameters miscellaneous help parameters
     * @param serviceClassFilesDir where to store the generated file
     * @throws CodeGenMakeException if generation fails
     */
    public static void generateImplementation(
            final CixsJaxwsService service,
            final Map < String, Object > parameters,
            final File serviceClassFilesDir)
            throws CodeGenMakeException {
        generateFile(JAXWS_TO_CIXS_GENERATOR_NAME,
                SERVICE_IMPLEMENTATION_VLC_TEMPLATE,
                SERVICE_MODEL_NAME,
                service,
                parameters,
                serviceClassFilesDir,
                service.getImplementationClassName() + ".java");
    }

    /**
     * Create the Jaxws Header class file.
     * 
     * @param service the Jaxws service description
     * @param parameters miscellaneous help parameters
     * @param serviceClassFilesDir where to store the generated file
     * @throws CodeGenMakeException if generation fails
     */
    public static void generateHeader(
            final CixsJaxwsService service,
            final Map < String, Object > parameters,
            final File serviceClassFilesDir)
            throws CodeGenMakeException {
        generateFile(JAXWS_TO_CIXS_GENERATOR_NAME,
                SERVICE_HEADER_VLC_TEMPLATE,
                SERVICE_MODEL_NAME,
                service,
                parameters,
                serviceClassFilesDir,
                service.getHeaderClassName() + ".java");
    }

    /**
     * Create the JAX-WS Ant Build War file.
     * 
     * @param service the Jaxws service description
     * @param parameters miscellaneous help parameters
     * @param serviceAntFilesDir where to store the generated file
     * @return the generated file name
     * @throws CodeGenMakeException if generation fails
     */
    public static String generateAntBuildWar(
            final CixsJaxwsService service,
            final Map < String, Object > parameters,
            final File serviceAntFilesDir)
            throws CodeGenMakeException {
        String fileName = "build-war.xml";
        generateFile(JAXWS_TO_CIXS_GENERATOR_NAME,
                SERVICE_ANT_BUILD_WAR_VLC_TEMPLATE,
                SERVICE_MODEL_NAME,
                service,
                parameters,
                serviceAntFilesDir,
                fileName,
                "UTF-8");
        return fileName;
    }

    /**
     * Create the JAX-WS Ant Build jar file.
     * 
     * @param service the Jaxws service description
     * @param parameters miscellaneous help parameters
     * @param serviceAntFilesDir where to store the generated file
     * @return the generated file name
     * @throws CodeGenMakeException if generation fails
     */
    public static String generateAntBuildJar(
            final CixsJaxwsService service,
            final Map < String, Object > parameters,
            final File serviceAntFilesDir)
            throws CodeGenMakeException {
        String fileName = "build-jar.xml";
        generateFile(JAXWS_TO_CIXS_GENERATOR_NAME,
                SERVICE_ANT_BUILD_JAR_VLC_TEMPLATE,
                SERVICE_MODEL_NAME,
                service,
                parameters,
                serviceAntFilesDir,
                fileName,
                "UTF-8");
        return fileName;
    }

    /**
     * Create the deploy Ant Build file.
     * 
     * @param service the Jaxws service description
     * @param parameters miscellaneous help parameters
     * @param serviceAntFilesDir where to store the generated file
     * @return the generated file name
     * @throws CodeGenMakeException if generation fails
     */
    public static String generateAntDeploy(
            final CixsJaxwsService service,
            final Map < String, Object > parameters,
            final File serviceAntFilesDir)
            throws CodeGenMakeException {
        String fileName = "deploy.xml";
        generateFile(JAXWS_GENERATOR_NAME,
                SERVICE_ANT_DEPLOY_VLC_TEMPLATE,
                SERVICE_MODEL_NAME,
                service,
                parameters,
                serviceAntFilesDir,
                fileName,
                "UTF-8");
        return fileName;
    }

    /**
     * Create the Jaxws Web Xml file.
     * 
     * @param service the Jaxws service description
     * @param parameters miscellaneous help parameters
     * @param serviceWebFilesDir where to store the generated file
     * @throws CodeGenMakeException if generation fails
     */
    public static void generateWebXml(
            final CixsJaxwsService service,
            final Map < String, Object > parameters,
            final File serviceWebFilesDir)
            throws CodeGenMakeException {
        generateFile(JAXWS_TO_CIXS_GENERATOR_NAME,
                SERVICE_WEB_XML_VLC_TEMPLATE,
                SERVICE_MODEL_NAME,
                service,
                parameters,
                serviceWebFilesDir,
                "web.xml",
                "UTF-8");
    }

    /**
     * Create the Jaxws Sun Jaxws Xml file.
     * 
     * @param service
     *            the Jaxws service description
     * @param parameters
     *            miscellaneous help parameters
     * @param serviceWebFilesDir
     *            where to store the generated file
     * @throws CodeGenMakeException
     *             if generation fails
     */
    public static void generateSunJaxwsXml(
            final CixsJaxwsService service,
            final Map < String, Object > parameters,
            final File serviceWebFilesDir)
            throws CodeGenMakeException {
        generateFile(JAXWS_TO_CIXS_GENERATOR_NAME,
                SERVICE_SUN_JAXWS_XML_VLC_TEMPLATE,
                SERVICE_MODEL_NAME,
                service,
                parameters,
                serviceWebFilesDir,
                "sun-jaxws.xml",
                "UTF-8");
    }

    /**
     * Create a fault class (Jaxws Exception).
     * 
     * @param operation the cixs operation
     * @param parameters miscellaneous help parameters
     * @param operationClassFilesDir where to store the generated file
     * @throws CodeGenMakeException if generation fails
     */
    public static void generateFault(
            final CixsOperation operation,
            final Map < String, Object > parameters,
            final File operationClassFilesDir)
            throws CodeGenMakeException {
        generateFile(JAXWS_TO_CIXS_GENERATOR_NAME,
                OPERATION_FAULT_VLC_TEMPLATE,
                "cixsOperation",
                operation,
                parameters,
                operationClassFilesDir,
                operation.getFaultType() + ".java");
    }

    /**
     * Create a fault info class.
     * 
     * @param operation the cixs operation
     * @param parameters miscellaneous help parameters
     * @param operationClassFilesDir where to store the generated file
     * @throws CodeGenMakeException if generation fails
     */
    public static void generateFaultInfo(
            final CixsOperation operation,
            final Map < String, Object > parameters,
            final File operationClassFilesDir)
            throws CodeGenMakeException {
        generateFile(JAXWS_TO_CIXS_GENERATOR_NAME,
                OPERATION_FAULT_INFO_VLC_TEMPLATE,
                "cixsOperation",
                operation,
                parameters,
                operationClassFilesDir,
                operation.getFaultInfoType() + ".java");
    }

    /**
     * Create a wrapper class.
     * 
     * @param operation the cixs operation
     * @param parameters miscellaneous help parameters
     * @param operationClassFilesDir where to store the generated file
     * @throws CodeGenMakeException if generation fails
     */
    public static void generateWrappers(
            final CixsOperation operation,
            final Map < String, Object > parameters,
            final File operationClassFilesDir)
            throws CodeGenMakeException {

        if (operation.getInput().size() > 0) {
            generateWrapper(operation, parameters, operationClassFilesDir,
                    operation.getRequestWrapperType(),
                    operation.getRequestHolderType(),
                    "Request", operation.getInput());
        }

        if (operation.getOutput().size() > 0) {
            generateWrapper(operation, parameters, operationClassFilesDir,
                    operation.getResponseWrapperType(),
                    operation.getResponseHolderType(),
                    "Response", operation.getOutput());
        }

    }

    /**
     * Generate a type of wrapper, either input or output, taking into
     * account multi-input and multi-output operations where the wrapper
     * actually wraps a holder.
     * 
     * @param operation the cixs operation
     * @param parameters miscellaneous help parameters
     * @param operationClassFilesDir where to store the generated file
     * @param wrapperType the Java class name for the wrapper
     * @param holderType the Java class name for the holder
     * @param propertyName either Request or Response
     * @param structures the list of either input or output structures
     * @throws CodeGenMakeException if generation fails
     */
    private static void generateWrapper(
            final CixsOperation operation,
            final Map < String, Object > parameters,
            final File operationClassFilesDir,
            final String wrapperType,
            final String holderType,
            final String propertyName,
            final List < CixsStructure > structures)
            throws CodeGenMakeException {

        try {
            parameters.put("propertyName", propertyName);
            parameters.put("fieldName", propertyName.toLowerCase(
                    Locale.getDefault()));
            parameters.put("wrapperType", wrapperType);

            if (structures.size() > 1) {
                parameters.put("fieldJaxbType", holderType);
                parameters.put("fieldJaxbNamespace",
                        parameters.get(
                                WebServiceParameters
                                .WSDL_TARGET_NAMESPACE_PROPERTY));
            } else {
                CixsStructure structure = structures.get(0);
                if (structure.getJaxbPackageName() != null
                        && structure.getJaxbPackageName().length() > 0) {
                    parameters.put("importType",
                            structure.getJaxbPackageName()
                                    + '.' + structure.getJaxbType());
                }
                parameters.put("fieldJaxbType", structure.getJaxbType());
                parameters.put("fieldJaxbNamespace", structure
                        .getJaxbNamespace());
            }

            generateFile(JAXWS_TO_CIXS_GENERATOR_NAME,
                    OPERATION_WRAPPER_VLC_TEMPLATE,
                    "cixsOperation",
                    operation,
                    parameters,
                    operationClassFilesDir,
                    wrapperType + ".java");

            /*
             * Remove local parameters from the parameters list so that
             * they don't interfere with a subsequent generation.
             */
            parameters.remove("propertyName");
            parameters.remove("fieldName");
            parameters.remove("wrapperType");
            parameters.remove("importType");
            parameters.remove("fieldJaxbType");
            parameters.remove("fieldJaxbNamespace");
        } catch (HostException e) {
            throw new CodeGenMakeException(e);
        }
    }

    /**
     * Create a holder classes for multi-structures input or output.
     * 
     * @param operation the cixs operation
     * @param parameters miscellaneous help parameters
     * @param operationClassFilesDir where to store the generated file
     * @throws CodeGenMakeException if generation fails
     */
    public static void generateHolders(
            final CixsOperation operation,
            final Map < String, Object > parameters,
            final File operationClassFilesDir)
            throws CodeGenMakeException {

        if (operation.getInput().size() > 1) {
            parameters.put("propertyName", "Request");
            generateFile(JAXWS_TO_CIXS_GENERATOR_NAME,
                    OPERATION_HOLDER_VLC_TEMPLATE,
                    "cixsOperation",
                    operation,
                    parameters,
                    operationClassFilesDir,
                    operation.getRequestHolderType() + ".java");
        }
        if (operation.getOutput().size() > 1) {
            parameters.put("propertyName", "Response");
            generateFile(JAXWS_TO_CIXS_GENERATOR_NAME,
                    OPERATION_HOLDER_VLC_TEMPLATE,
                    "cixsOperation",
                    operation,
                    parameters,
                    operationClassFilesDir,
                    operation.getResponseHolderType() + ".java");
        }
    }

    /**
     * Create a program invoker class.
     * 
     * @param operation the cixs operation
     * @param parameters miscellaneous help parameters
     * @param operationClassFilesDir where to store the generated file
     * @throws CodeGenMakeException if generation fails
     */
    public static void generateProgramInvoker(
            final CixsOperation operation,
            final Map < String, Object > parameters,
            final File operationClassFilesDir)
            throws CodeGenMakeException {
        generateFile(JAXWS_TO_CIXS_GENERATOR_NAME,
                OPERATION_PROGRAM_INVOKER_VLC_TEMPLATE,
                "cixsOperation",
                operation,
                parameters,
                operationClassFilesDir,
                operation.getClassName() + "ProgramInvoker.java");
    }

    /**
     * Create the host program bean.
     * 
     * @param operation the cixs operation
     * @param parameters miscellaneous help parameters
     * @param operationClassFilesDir where to store the generated file
     * @throws CodeGenMakeException if generation fails
     */
    public static void generateHostProgram(
            final CixsOperation operation,
            final Map < String, Object > parameters,
            final File operationClassFilesDir)
            throws CodeGenMakeException {
        generateFile(JAXWS_TO_CIXS_GENERATOR_NAME,
                OPERATION_HOST_PROGRAM_VLC_TEMPLATE,
                "cixsOperation",
                operation,
                parameters,
                operationClassFilesDir,
                operation.getClassName() + "HostProgram.java");
    }

    /**
     * Create the Jaxb package-info.java class.
     * 
     * @param service the Jaxws service description
     * @param parameters miscellaneous help parameters
     * @param serviceClassFilesDir where to store the generated file
     * @return the generated local file name
     * @throws CodeGenMakeException if generation fails
     */
    public static String generatePackageInfo(
            final CixsJaxwsService service,
            final Map < String, Object > parameters,
            final File serviceClassFilesDir)
            throws CodeGenMakeException {
        String fileName = "package-info.java";
        generateFile(JAXWS_TO_CIXS_GENERATOR_NAME,
                SERVICE_PACKAGE_INFO_VLC_TEMPLATE,
                SERVICE_MODEL_NAME,
                service,
                parameters,
                serviceClassFilesDir,
                fileName);
        return fileName;
    }

    /**
     * Create the Jaxb ObjectFctory.java class.
     * 
     * @param service the Jaxws service description
     * @param parameters miscellaneous help parameters
     * @param serviceClassFilesDir where to store the generated file
     * @return the generated local file name
     * @throws CodeGenMakeException if generation fails
     */
    public static String generateObjectFactory(
            final CixsJaxwsService service,
            final Map < String, Object > parameters,
            final File serviceClassFilesDir)
            throws CodeGenMakeException {
        String fileName = "ObjectFactory.java";
        generateFile(JAXWS_TO_CIXS_GENERATOR_NAME,
                SERVICE_OBJECTFACTORY_VLC_TEMPLATE,
                SERVICE_MODEL_NAME,
                service,
                parameters,
                serviceClassFilesDir,
                fileName);
        return fileName;
    }

    /**
     * {@inheritDoc}
     * 
     * @see com.legstar.cixs.gen.ant.AbstractCixsGenerator#getAntModel()
     */
    public AntBuildJaxws2CixsModel getAntModel() {
        return (AntBuildJaxws2CixsModel) super.getAntModel();
    }

    /** {@inheritDoc} */
    @Override
    public String getGeneratorName() {
        return JAXWS_TO_CIXS_GENERATOR_NAME;
    }

    /**
     * @return the set of parameters needed to expose a Web Service
     */
    public WebServiceParameters getWebServiceParameters() {
        return getAntModel().getWebServiceParameters();
    }

    /**
     * @param webServiceParameters the set of parameters needed to expose a Web
     *            Service to set
     */
    public void setWebServiceParameters(
            final WebServiceParameters webServiceParameters) {
        getAntModel().setWebServiceParameters(webServiceParameters);
    }

    /**
     * @param webServiceParameters the set of parameters needed to expose a Web
     *            Service to set
     */
    public void addWebServiceParameters(
            final WebServiceParameters webServiceParameters) {
        getAntModel().setWebServiceParameters(webServiceParameters);
    }

}

package com.legstar.cixs.jaxws.gen;

import java.io.File;
import java.util.List;
import java.util.Map;

import com.legstar.cixs.gen.ant.AbstractCixsGenerator;
import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.gen.model.CixsStructure;
import com.legstar.cixs.jaxws.model.AntBuildJaxws2CixsModel;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.CodeGenUtil;

/**
 * This Ant task creates the various Jaxws artifacts needed to implement
 * a Jaxws Web Service that acts as an adapter for a mainframe program.
 * Web service clients can consume this adapter as any other Web Service
 * but internally the adapter use the LegStar transport to call a 
 * a mainframe program.
 */
public class Jaxws2CixsGenerator extends AbstractCixsGenerator {

	/** This generator name. */
	private static final String CIXS_JAXWS_GENERATOR_NAME =
		"LegStar Jaxws Service generator";

	/** Velocity template for component interface. */
	public static final String COMPONENT_INTERFACE_VLC_TEMPLATE =
		"vlc/cixsjaxws-component-interface.vm";
	
	/** Velocity template for component implementation. */
	public static final String COMPONENT_IMPLEMENTATION_VLC_TEMPLATE =
		"vlc/cixsjaxws-component-implementation.vm";

	/** Velocity template for component header. */
	public static final String COMPONENT_HEADER_VLC_TEMPLATE =
		"vlc/cixsjaxws-component-header.vm";

	/** Velocity template for component ant-build-war. */
	public static final String COMPONENT_ANT_BUILD_WAR_VLC_TEMPLATE =
		"vlc/cixsjaxws-component-ant-build-war-xml.vm";

	/** Velocity template for component sun-jaxws-xml. */
	public static final String COMPONENT_SUN_JAXWS_XML_VLC_TEMPLATE =
		"vlc/cixsjaxws-component-sun-jaxws-xml.vm";

	/** Velocity template for component web-xml. */
	public static final String COMPONENT_WEB_XML_VLC_TEMPLATE =
		"vlc/cixsjaxws-component-web-xml.vm";

	/** Velocity template for fault. */
	public static final String OPERATION_FAULT_VLC_TEMPLATE =
		"vlc/cixsjaxws-operation-fault.vm";

	/** Velocity template for fault info. */
	public static final String OPERATION_FAULT_INFO_VLC_TEMPLATE =
		"vlc/cixsjaxws-operation-fault-info.vm";

	/** Velocity template for holder. */
	public static final String OPERATION_HOLDER_VLC_TEMPLATE =
		"vlc/cixsjaxws-operation-holder.vm";

	/** Velocity template for program. */
	public static final String OPERATION_PROGRAM_VLC_TEMPLATE =
		"vlc/cixsjaxws-operation-program.vm";

	/** Velocity template for wrapper. */
	public static final String OPERATION_WRAPPER_VLC_TEMPLATE =
		"vlc/cixsjaxws-operation-wrapper.vm";

	/**
     * Constructor.
     */
    public Jaxws2CixsGenerator() {
        super(new AntBuildJaxws2CixsModel());
    }
    
    /**
     * Check that input values are valid.
     * @throws CodeGenMakeException if input is invalid
     */
    public void checkExtendedInput() throws CodeGenMakeException {
        try {
			CodeGenUtil.checkDirectory(getTargetWDDDir(), true);
            CodeGenUtil.checkHttpURI(getHostURI());
        } catch (IllegalArgumentException e) {
            throw new CodeGenMakeException(e);
        }
    }
    
	/**
	 * Generate default values where they are missing in the model. This
	 * will reduce the amount of code in the velocity templates.
	 */
	protected void completeModel() {
		for (CixsOperation operation : getCixsService().getCixsOperations()) {
			if (operation.getNamespace() == null 
					|| operation.getNamespace().length() == 0) {
				operation.setNamespace(
						getCixsJaxwsService().getTargetNamespace());
			}
			if (operation.getPackageName() == null 
					|| operation.getPackageName().length() == 0) {
				operation.setPackageName(
						getCixsJaxwsService().getPackageName());
			}
		}
	}
   /**
     * Create all artifacts for a Jaxws Web Service.
     * @param parameters a predefined set of parameters useful for generation
     * @throws CodeGenMakeException if generation fails
     */
    public void generate(
    		final Map < String, Object > parameters)
    throws CodeGenMakeException {

        parameters.put("targetWarDir", getTargetWarDir());
        parameters.put("targetWDDDir", getTargetWDDDir());
        parameters.put("hostCharset", getHostCharset());
        parameters.put("hostURI", getHostURI());

		/* Determine target files locations */
        File componentClassFilesDir = CodeGenUtil.classFilesLocation(
				getTargetSrcDir(), getCixsService().getPackageName(), true);
		File componentWebFilesDir =	getTargetWDDDir();
		CodeGenUtil.checkDirectory(componentWebFilesDir, true);
		File componentAntFilesDir =	getTargetAntDir();
		CodeGenUtil.checkDirectory(componentAntFilesDir, true);
		
		/* Produce artifacts */
		generateInterface(
				getCixsJaxwsService(), parameters, componentClassFilesDir);
		generateImplementation(
				getCixsJaxwsService(), parameters, componentClassFilesDir);
		generateHeader(
				getCixsJaxwsService(), parameters, componentClassFilesDir);
		generateSunJaxwsXml(
				getCixsJaxwsService(), parameters, componentWebFilesDir);
		generateWebXml(
				getCixsJaxwsService(), parameters, componentWebFilesDir);
		generateAntBuildWar(
				getCixsJaxwsService(), parameters, componentAntFilesDir);
		
		for (CixsOperation operation : getCixsService().getCixsOperations()) {

			/* Determine target files locations */
			File operationClassFilesDir = CodeGenUtil.classFilesLocation(
					getTargetSrcDir(), operation.getPackageName(), true);
			File operationPropertiesFilesDir = getTargetPropDir();
			CodeGenUtil.checkDirectory(operationPropertiesFilesDir, true);
			
			generateFault(
					operation, parameters, operationClassFilesDir);
			generateFaultInfo(
					operation, parameters, operationClassFilesDir);
			generateWrappers(
					operation, parameters, operationClassFilesDir);
			generateHolders(
					operation, parameters, operationClassFilesDir);
			generateProgramProperties(
					operation, parameters, operationPropertiesFilesDir);
			
		}
    }
    
	/**
	 * Create the Jaxws Interface class file.
	 * @param service the Jaxws service description
	 * @param parameters miscellaneous help parameters
	 * @param componentClassFilesDir where to store the generated file
	 * @throws CodeGenMakeException if generation fails
	 */
	public static void generateInterface(
			final CixsJaxwsService service,
			final Map < String, Object > parameters,
			final File componentClassFilesDir)
	throws CodeGenMakeException {
        generateFile(CIXS_JAXWS_GENERATOR_NAME,
        		COMPONENT_INTERFACE_VLC_TEMPLATE,
                "jaxwsComponent",
                service,
                parameters,
                componentClassFilesDir,
                service.getInterfaceClassName() + ".java");
	}
	
	/**
	 * Create the Jaxws Implementation class file.
	 * @param service the Jaxws service description
	 * @param parameters miscellaneous help parameters
	 * @param componentClassFilesDir where to store the generated file
	 * @throws CodeGenMakeException if generation fails
	 */
	public static void generateImplementation(
			final CixsJaxwsService service,
			final Map < String, Object > parameters,
			final File componentClassFilesDir)
	throws CodeGenMakeException {
        generateFile(CIXS_JAXWS_GENERATOR_NAME,
        		COMPONENT_IMPLEMENTATION_VLC_TEMPLATE,
                "jaxwsComponent",
                service,
                parameters,
                componentClassFilesDir,
                service.getImplementationClassName() + ".java");
	}
	
	/**
	 * Create the Jaxws Header class file.
	 * @param service the Jaxws service description
	 * @param parameters miscellaneous help parameters
	 * @param componentClassFilesDir where to store the generated file
	 * @throws CodeGenMakeException if generation fails
	 */
	public static void generateHeader(
			final CixsJaxwsService service,
			final Map < String, Object > parameters,
			final File componentClassFilesDir)
	throws CodeGenMakeException {
        generateFile(CIXS_JAXWS_GENERATOR_NAME,
        		COMPONENT_HEADER_VLC_TEMPLATE,
                "jaxwsComponent",
                service,
                parameters,
                componentClassFilesDir,
                service.getHeaderClassName() + ".java");
	}
	
	/**
	 * Create the Jaxws Ant Build War file.
	 * @param service the Jaxws service description
	 * @param parameters miscellaneous help parameters
	 * @param componentAntFilesDir where to store the generated file
	 * @throws CodeGenMakeException if generation fails
	 */
	public static void generateAntBuildWar(
			final CixsJaxwsService service,
			final Map < String, Object > parameters,
			final File componentAntFilesDir)
	throws CodeGenMakeException {
        generateFile(CIXS_JAXWS_GENERATOR_NAME,
        		COMPONENT_ANT_BUILD_WAR_VLC_TEMPLATE,
                "jaxwsComponent",
                service,
                parameters,
                componentAntFilesDir,
                "build.xml");
	}
	
	/**
	 * Create the Jaxws Web Xml file.
	 * @param service the Jaxws service description
	 * @param parameters miscellaneous help parameters
	 * @param componentWebFilesDir where to store the generated file
	 * @throws CodeGenMakeException if generation fails
	 */
	public static void generateWebXml(
			final CixsJaxwsService service,
			final Map < String, Object > parameters,
			final File componentWebFilesDir)
	throws CodeGenMakeException {
        generateFile(CIXS_JAXWS_GENERATOR_NAME,
        		COMPONENT_WEB_XML_VLC_TEMPLATE,
                "jaxwsComponent",
                service,
                parameters,
                componentWebFilesDir,
                "web.xml");
	}
	
	/**
	 * Create the Jaxws Sun Jaxws Xml file.
	 * @param service the Jaxws service description
	 * @param parameters miscellaneous help parameters
	 * @param componentWebFilesDir where to store the generated file
	 * @throws CodeGenMakeException if generation fails
	 */
	public static void generateSunJaxwsXml(
			final CixsJaxwsService service,
			final Map < String, Object > parameters,
			final File componentWebFilesDir)
	throws CodeGenMakeException {
        generateFile(CIXS_JAXWS_GENERATOR_NAME,
        		COMPONENT_SUN_JAXWS_XML_VLC_TEMPLATE,
                "jaxwsComponent",
                service,
                parameters,
                componentWebFilesDir,
                "sun-jaxws.xml");
	}
	
	/**
	 * Create the Propram properties file.
	 * @param operation the cixs operation
	 * @param parameters miscellaneous help parameters
	 * @param componentPropertiesDir where to store the generated file
	 * @throws CodeGenMakeException if generation fails
	 */
	public static void generateProgramProperties(
			final CixsOperation operation,
			final Map < String, Object > parameters,
			final File componentPropertiesDir)
	throws CodeGenMakeException {
        generateFile(CIXS_JAXWS_GENERATOR_NAME,
                OPERATION_PROGRAM_VLC_TEMPLATE,
                "cixsOperation",
                operation,
                parameters,
                componentPropertiesDir,
                operation.getCicsProgramName().toLowerCase() + ".properties");
	}
	
	/**
	 * Create a fault class (Jaxws Exception).
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
        generateFile(CIXS_JAXWS_GENERATOR_NAME,
        		OPERATION_FAULT_VLC_TEMPLATE,
                "cixsOperation",
                operation,
                parameters,
                operationClassFilesDir,
                operation.getFaultType() + ".java");
	}

	/**
	 * Create a fault info class.
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
        generateFile(CIXS_JAXWS_GENERATOR_NAME,
        		OPERATION_FAULT_INFO_VLC_TEMPLATE,
                "cixsOperation",
                operation,
                parameters,
                operationClassFilesDir,
                operation.getFaultInfoType() + ".java");
	}

	/**
	 * Create a wrapper class.
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
		
		parameters.put("propertyName", propertyName);
		parameters.put("fieldName", propertyName.toLowerCase());
		parameters.put("wrapperType", wrapperType);
		
		if (operation.getCicsChannel() == null
				|| operation.getCicsChannel().length() == 0) {
			CixsStructure structure = structures.get(0);
			if (structure.getJaxbPackageName() != null
					&& structure.getJaxbPackageName().length() > 0) {
				parameters.put("importType",
						structure.getJaxbPackageName()
						+ '.' + structure.getJaxbType());
			}
			parameters.put("fieldType", structure.getJaxbType());
		} else {
			parameters.put("fieldType",	holderType);
		}

        generateFile(CIXS_JAXWS_GENERATOR_NAME,
        		OPERATION_WRAPPER_VLC_TEMPLATE,
                "cixsOperation",
                operation,
                parameters,
                operationClassFilesDir,
                wrapperType + ".java");
	}

	/**
	 * Create a holder classes for channel/containers.
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

		if (operation.getCicsChannel() == null
				|| operation.getCicsChannel().length() == 0) {
			return;
		}
		
		if (operation.getInput().size() > 0) {
			parameters.put("propertyName", "Request");
	        generateFile(CIXS_JAXWS_GENERATOR_NAME,
	        		OPERATION_HOLDER_VLC_TEMPLATE,
	                "cixsOperation",
	                operation,
	                parameters,
	                operationClassFilesDir,
	                operation.getRequestHolderType() + ".java");
		}
		if (operation.getOutput().size() > 0) {
			parameters.put("propertyName", "Response");
	        generateFile(CIXS_JAXWS_GENERATOR_NAME,
	        		OPERATION_HOLDER_VLC_TEMPLATE,
	                "cixsOperation",
	                operation,
	                parameters,
	                operationClassFilesDir,
	                operation.getResponseHolderType() + ".java");
		}
	}

    /**
     * {@inheritDoc}
     * @see com.legstar.cixs.gen.ant.AbstractCixsGenerator#getModel()
     */
    public AntBuildJaxws2CixsModel getModel() {
        return (AntBuildJaxws2CixsModel) super.getModel();
    }

    /**
     * @return the URI that the host exposes to consumers
     */
    public final String getHostURI() {
        return getModel().getHostURI();
    }

    /**
     * @param hostURI the URI that the host exposes to consumers to set
     */
    public final void setHostURI(final String hostURI) {
        getModel().setHostURI(hostURI);
    }

	/**
	 * @return the Target location for web deployment descriptors
	 */
	public final File getTargetWDDDir() {
		return getModel().getTargetWDDDir();
	}

	/**
	 * @param targetWDDDir the Target location for web deployment descriptors to
	 *  set
	 */
	public final void setTargetWDDDir(final File targetWDDDir) {
		getModel().setTargetWDDDir(targetWDDDir);
	}

	/**
	 * @return the deployment location for jaxws war files
	 */
	public final File getTargetWarDir() {
		return getModel().getTargetWarDir();
	}

	/**
	 * @param targetWarDir the deployment location for jaxws war files to set
	 */
	public final void setTargetWarDir(final File targetWarDir) {
		getModel().setTargetWarDir(targetWarDir);
	}
	
	/**
	 * @return the service description
	 */
	public final CixsJaxwsService getCixsJaxwsService() {
		return (CixsJaxwsService) getCixsService();
	}
	
	/**
	 * @param cixsJaxwsService the service description to set
	 */
	public final void setCixsJaxwsService(
			final CixsJaxwsService cixsJaxwsService) {
		this.setCixsService(cixsJaxwsService);
	}

	/**
	 * @param cixsJaxwsService the Jaxws service to set
	 */
	public final void add(final CixsJaxwsService cixsJaxwsService) {
		setCixsService(cixsJaxwsService);
	}

	/**
	 * @param cixsJaxwsService the Jaxws service to set
	 */
	public final void addCixsJaxwsService(
			final CixsJaxwsService cixsJaxwsService) {
		setCixsService(cixsJaxwsService);
	}

    /**
     * @deprecated use <code>getTargetBinDir</code> instead.
     * @return the Service binaries
     */
    public final File getCixsBinDir() {
        return getModel().getTargetBinDir();
    }

    /**
     * @deprecated use <code>setTargetBinDir</code> instead.
     * @param targetBinDir the Service binaries to set
     */
    public final void setCixsBinDir(final File targetBinDir) {
    	getModel().setTargetBinDir(targetBinDir);
    }

	/** {@inheritDoc}*/
	@Override
	public String getGeneratorName() {
		return CIXS_JAXWS_GENERATOR_NAME;
	}
}

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
package com.legstar.cixs.jaxws.gen;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.gen.model.CixsStructure;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.codegen.CodeGenHelper;
import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.CodeGenUtil;

/**
 * This Ant task creates the various Jaxws Service artifacts needed
 * for a complete Jaxws service with LegStar access capabilities.
 */
public class CixsJaxwsGenerator extends Task {

	/** Service descriptor. */
	private CixsJaxwsService mCixsJaxwsService;

	/** Target location for generated source. */
	private String mTargetSrcDir;

	/** Target location for ant deployment script. */
	private String mTargetAntDir;
	
	/** Target location for web deployment descriptors. */
	private String mTargetWDDDir;
	
	/** Target location for properties files. */
	private String mTargetPropDir;
	
	/** Target location for web war files. */
	private String mTargetWarDir;
	
	/** Location of jaxb classes binaries. */
	private String mJaxbBinDir;
	
	/** Location of jaxb classes binaries. */
	private String mCoxbBinDir;
	
	/** Location of Jaxws endpoint binaries. */
	private String mCixsBinDir;
	
	/** Location of custom binaries. */
	private String mCustBinDir;
	
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

	/** Logger. */
	private static final Log LOG = LogFactory.getLog(CixsJaxwsGenerator.class);

	/** @{inheritDoc}*/
	@Override
	public final void init() {
		LOG.info("Initializing Jaxws service generator");
		try {
			CodeGenUtil.initVelocity();
		} catch (Exception e) {
			throw new BuildException(e.getMessage());
		}
	}

	/**
	 * Check that enough input parameters are set and then
	 * generate the requested artifacts.
	 * 
	 * */
	@Override
	public final void execute() {
		LOG.info("Generating Jaxws service artifacts for "
				+ ((mCixsJaxwsService == null) ? "null"
						: mCixsJaxwsService.getName()));
		long start = System.currentTimeMillis();

		try {
			checkInput();
			generate();

		} catch (CodeGenMakeException e) {
			LOG.error("Jaxws service generator failure", e);
			throw new BuildException(e);
		}

		long end = System.currentTimeMillis();
		LOG.info("Generation success for " + mCixsJaxwsService.getName());
		LOG.info("Duration = " + (end - start) + " ms");
	}

	/**
	 * Check that input values are valid.
	 * @throws CodeGenMakeException if input is invalid
	 */
	private void checkInput() throws CodeGenMakeException {
		if (mCixsJaxwsService == null) {
			throw new CodeGenMakeException(
					"Missing cixs Jaxws service parameter");
		}
		try {
			CodeGenUtil.checkDirectory(mTargetSrcDir, true);
			CodeGenUtil.checkDirectory(mTargetAntDir, true);
			CodeGenUtil.checkDirectory(mTargetWDDDir, true);
			CodeGenUtil.checkDirectory(mTargetPropDir, true);
		} catch (IllegalArgumentException e) {
			throw new CodeGenMakeException(e);
		}
	}
	
	/**
	 * Create all artifacts for Jaxws endpoint.
	 * @throws CodeGenMakeException if generation fails
	 */
	private void generate() throws CodeGenMakeException {
		Map < String, Object > parameters = new HashMap < String, Object >();
		CodeGenHelper helper = new CodeGenHelper();
		parameters.put("helper", helper);
		CixsHelper cixsHelper = new CixsHelper();
		parameters.put("cixsHelper", cixsHelper);

		/* These parameters are primarily useful for the ant build template */
		parameters.put("warDir", getTargetWarDir());
    	parameters.put("wddDir", getTargetWDDDir());
    	parameters.put("jaxbBinDir", getJaxbBinDir());
    	parameters.put("coxbBinDir", getCoxbBinDir());
    	parameters.put("cixsBinDir", getCixsBinDir());
    	parameters.put("custBinDir", getCustBinDir());
    	parameters.put("propDir", getTargetPropDir());

		/* Determine target files locations */
		String componentClassFilesLocation = CodeGenUtil.classFilesLocation(
				mTargetSrcDir, mCixsJaxwsService.getPackageName());
		String componentWebFilesLocation =
			getTargetWDDDir();
		CodeGenUtil.checkDirectory(componentWebFilesLocation, true);
		String componentAntFilesLocation =
			getTargetAntDir();
		CodeGenUtil.checkDirectory(componentAntFilesLocation, true);
		
		/* Produce artifacts */
		generateInterface(
				mCixsJaxwsService, parameters, componentClassFilesLocation);
		generateImplementation(
				mCixsJaxwsService, parameters, componentClassFilesLocation);
		generateHeader(
				mCixsJaxwsService, parameters, componentClassFilesLocation);
		generateSunJaxwsXml(
				mCixsJaxwsService, parameters, componentWebFilesLocation);
		generateWebXml(
				mCixsJaxwsService, parameters, componentWebFilesLocation);
		generateAntBuildWar(
				mCixsJaxwsService, parameters, componentAntFilesLocation);
		
		for (CixsOperation operation : mCixsJaxwsService.getCixsOperations()) {
			String operationNamespace = cixsHelper.getOperationNamespace(
					operation, mCixsJaxwsService.getTargetNamespace());
	    	parameters.put("operationNamespace", operationNamespace);
	    	String operationPackageName = cixsHelper.getOperationPackageName(
	    			operation, mCixsJaxwsService.getPackageName());
	    	parameters.put("operationPackageName", operationPackageName);

			/* Determine target files locations */
			String operationClassFilesLocation = CodeGenUtil.classFilesLocation(
					mTargetSrcDir, operationPackageName);
			String operationPropertiesFilesLocation = getTargetPropDir();
			CodeGenUtil.checkDirectory(operationPropertiesFilesLocation, true);
			
			generateFault(
					operation, parameters, operationClassFilesLocation);
			generateFaultInfo(
					operation, parameters, operationClassFilesLocation);
			generateWrappers(
					operation, parameters, operationClassFilesLocation);
			generateHolders(
					operation, parameters, operationClassFilesLocation);
			generateProgramProperties(
					operation, parameters, operationPropertiesFilesLocation);
			
		}
		
	}

	/**
	 * Create the Jaxws Interface class file.
	 * @param service the Jaxws service description
	 * @param parameters miscellaneous help parameters
	 * @param componentClassFilesLocation where to store the generated file
	 * @throws CodeGenMakeException if generation fails
	 */
	public static void generateInterface(
			final CixsJaxwsService service,
			final Map < String, Object > parameters,
			final String componentClassFilesLocation)
	throws CodeGenMakeException {

		File targetFile = CodeGenUtil.getFile(componentClassFilesLocation,
				service.getInterfaceClassName() + ".java");
		LOG.info("Generating " + targetFile.getAbsolutePath());
		CodeGenUtil.processTemplate(
				CIXS_JAXWS_GENERATOR_NAME,
				COMPONENT_INTERFACE_VLC_TEMPLATE,
				"jaxwsComponent", service,
				parameters,
				targetFile);
	}
	
	/**
	 * Create the Jaxws Implementation class file.
	 * @param service the Jaxws service description
	 * @param parameters miscellaneous help parameters
	 * @param componentClassFilesLocation where to store the generated file
	 * @throws CodeGenMakeException if generation fails
	 */
	public static void generateImplementation(
			final CixsJaxwsService service,
			final Map < String, Object > parameters,
			final String componentClassFilesLocation)
	throws CodeGenMakeException {

		File targetFile = CodeGenUtil.getFile(componentClassFilesLocation,
				service.getImplementationClassName() + ".java");
		LOG.info("Generating " + targetFile.getAbsolutePath());
		CodeGenUtil.processTemplate(
				CIXS_JAXWS_GENERATOR_NAME,
				COMPONENT_IMPLEMENTATION_VLC_TEMPLATE,
				"jaxwsComponent", service,
				parameters,
				targetFile);
	}
	
	/**
	 * Create the Jaxws Header class file.
	 * @param service the Jaxws service description
	 * @param parameters miscellaneous help parameters
	 * @param componentClassFilesLocation where to store the generated file
	 * @throws CodeGenMakeException if generation fails
	 */
	public static void generateHeader(
			final CixsJaxwsService service,
			final Map < String, Object > parameters,
			final String componentClassFilesLocation)
	throws CodeGenMakeException {

		File targetFile = CodeGenUtil.getFile(componentClassFilesLocation,
				service.getHeaderClassName() + ".java");
		LOG.info("Generating " + targetFile.getAbsolutePath());
		CodeGenUtil.processTemplate(
				CIXS_JAXWS_GENERATOR_NAME,
				COMPONENT_HEADER_VLC_TEMPLATE,
				"jaxwsComponent", service,
				parameters,
				targetFile);
	}
	
	/**
	 * Create the Jaxws Ant Build War file.
	 * @param service the Jaxws service description
	 * @param parameters miscellaneous help parameters
	 * @param componentAntFilesLocation where to store the generated file
	 * @throws CodeGenMakeException if generation fails
	 */
	public static void generateAntBuildWar(
			final CixsJaxwsService service,
			final Map < String, Object > parameters,
			final String componentAntFilesLocation)
	throws CodeGenMakeException {

		File targetFile = CodeGenUtil.getFile(componentAntFilesLocation,
		"build.xml");
		LOG.info("Generating " + targetFile.getAbsolutePath());
		CodeGenUtil.processTemplate(
				CIXS_JAXWS_GENERATOR_NAME,
				COMPONENT_ANT_BUILD_WAR_VLC_TEMPLATE,
				"jaxwsComponent", service,
				parameters,
				targetFile);
	}
	
	/**
	 * Create the Jaxws Web Xml file.
	 * @param service the Jaxws service description
	 * @param parameters miscellaneous help parameters
	 * @param componentWebFilesLocation where to store the generated file
	 * @throws CodeGenMakeException if generation fails
	 */
	public static void generateWebXml(
			final CixsJaxwsService service,
			final Map < String, Object > parameters,
			final String componentWebFilesLocation)
	throws CodeGenMakeException {

		File targetFile = CodeGenUtil.getFile(componentWebFilesLocation,
		"web.xml");
		LOG.info("Generating " + targetFile.getAbsolutePath());
		CodeGenUtil.processTemplate(
				CIXS_JAXWS_GENERATOR_NAME,
				COMPONENT_WEB_XML_VLC_TEMPLATE,
				"jaxwsComponent", service,
				parameters,
				targetFile);
	}
	
	/**
	 * Create the Jaxws Sun Jaxws Xml file.
	 * @param service the Jaxws service description
	 * @param parameters miscellaneous help parameters
	 * @param componentWebFilesLocation where to store the generated file
	 * @throws CodeGenMakeException if generation fails
	 */
	public static void generateSunJaxwsXml(
			final CixsJaxwsService service,
			final Map < String, Object > parameters,
			final String componentWebFilesLocation)
	throws CodeGenMakeException {

		File targetFile = CodeGenUtil.getFile(componentWebFilesLocation,
		"sun-jaxws.xml");
		LOG.info("Generating " + targetFile.getAbsolutePath());
		CodeGenUtil.processTemplate(
				CIXS_JAXWS_GENERATOR_NAME,
				COMPONENT_SUN_JAXWS_XML_VLC_TEMPLATE,
				"jaxwsComponent", service,
				parameters,
				targetFile);
	}
	
	/**
	 * Create the Propram properties file.
	 * @param operation the cixs operation
	 * @param parameters miscellaneous help parameters
	 * @param componentPropertiesLocation where to store the generated file
	 * @throws CodeGenMakeException if generation fails
	 */
	public static void generateProgramProperties(
			final CixsOperation operation,
			final Map < String, Object > parameters,
			final String componentPropertiesLocation)
	throws CodeGenMakeException {

		File targetFile = CodeGenUtil.getFile(componentPropertiesLocation,
				operation.getCicsProgramName().toLowerCase() + ".properties");
		LOG.info("Generating " + targetFile.getAbsolutePath());
		CodeGenUtil.processTemplate(
				CIXS_JAXWS_GENERATOR_NAME,
				OPERATION_PROGRAM_VLC_TEMPLATE,
				"cixsOperation", operation,
				parameters,
				targetFile);
	}
	
	/**
	 * Create a fault class (Jaxws Exception).
	 * @param operation the cixs operation
	 * @param parameters miscellaneous help parameters
	 * @param operationClassFilesLocation where to store the generated file
	 * @throws CodeGenMakeException if generation fails
	 */
	public static void generateFault(
			final CixsOperation operation,
			final Map < String, Object > parameters,
			final String operationClassFilesLocation)
	throws CodeGenMakeException {

		File targetFile = CodeGenUtil.getFile(operationClassFilesLocation,
				operation.getFaultType() + ".java");
		LOG.info("Generating " + targetFile.getAbsolutePath());
		CodeGenUtil.processTemplate(
				CIXS_JAXWS_GENERATOR_NAME,
				OPERATION_FAULT_VLC_TEMPLATE,
				"cixsOperation", operation,
				parameters,
				targetFile);
	}

	/**
	 * Create a fault info class.
	 * @param operation the cixs operation
	 * @param parameters miscellaneous help parameters
	 * @param operationClassFilesLocation where to store the generated file
	 * @throws CodeGenMakeException if generation fails
	 */
	public static void generateFaultInfo(
			final CixsOperation operation,
			final Map < String, Object > parameters,
			final String operationClassFilesLocation)
	throws CodeGenMakeException {

		File targetFile = CodeGenUtil.getFile(operationClassFilesLocation,
				operation.getFaultInfoType() + ".java");
		LOG.info("Generating " + targetFile.getAbsolutePath());
		CodeGenUtil.processTemplate(
				CIXS_JAXWS_GENERATOR_NAME,
				OPERATION_FAULT_INFO_VLC_TEMPLATE,
				"cixsOperation", operation,
				parameters,
				targetFile);
	}

	/**
	 * Create a wrapper class.
	 * @param operation the cixs operation
	 * @param parameters miscellaneous help parameters
	 * @param operationClassFilesLocation where to store the generated file
	 * @throws CodeGenMakeException if generation fails
	 */
	public static void generateWrappers(
			final CixsOperation operation,
			final Map < String, Object > parameters,
			final String operationClassFilesLocation)
	throws CodeGenMakeException {
		
		if (operation.getInput().size() > 0) {
			generateWrapper(operation, parameters, operationClassFilesLocation,
					operation.getRequestWrapperType(),
					operation.getRequestHolderType(),
					"Request", operation.getInput());
		}

		if (operation.getOutput().size() > 0) {
			generateWrapper(operation, parameters, operationClassFilesLocation,
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
	 * @param operationClassFilesLocation where to store the generated file
	 * @param wrapperType the Java class name for the wrapper
	 * @param holderType the Java class name for the holder
	 * @param propertyName either Request or Response
	 * @param structures the list of either input or output structures
	 * @throws CodeGenMakeException if generation fails
	 */
	private static void generateWrapper(
			final CixsOperation operation,
			final Map < String, Object > parameters,
			final String operationClassFilesLocation,
			final String wrapperType,
			final String holderType,
			final String propertyName,
			final List < CixsStructure > structures)
	throws CodeGenMakeException {
		
		File targetFile = CodeGenUtil.getFile(operationClassFilesLocation,
				wrapperType + ".java");
		LOG.info("Generating " + targetFile.getAbsolutePath());
		
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
		CodeGenUtil.processTemplate(
				CIXS_JAXWS_GENERATOR_NAME,
				OPERATION_WRAPPER_VLC_TEMPLATE,
				"cixsOperation", operation,
				parameters,
				targetFile);
	}

	/**
	 * Create a holder classes for channel/containers.
	 * @param operation the cixs operation
	 * @param parameters miscellaneous help parameters
	 * @param operationClassFilesLocation where to store the generated file
	 * @throws CodeGenMakeException if generation fails
	 */
	public static void generateHolders(
			final CixsOperation operation,
			final Map < String, Object > parameters,
			final String operationClassFilesLocation)
	throws CodeGenMakeException {

		if (operation.getCicsChannel() == null
				|| operation.getCicsChannel().length() == 0) {
			return;
		}
		
		if (operation.getInput().size() > 0) {
			File targetFile = CodeGenUtil.getFile(operationClassFilesLocation,
					operation.getRequestHolderType() + ".java");
			LOG.info("Generating " + targetFile.getAbsolutePath());
			parameters.put("propertyName", "Request");
			CodeGenUtil.processTemplate(
					CIXS_JAXWS_GENERATOR_NAME,
					OPERATION_HOLDER_VLC_TEMPLATE,
					"cixsOperation", operation,
					parameters,
					targetFile);
		}
		if (operation.getOutput().size() > 0) {
			File targetFile = CodeGenUtil.getFile(operationClassFilesLocation,
					operation.getResponseHolderType() + ".java");
			LOG.info("Generating " + targetFile.getAbsolutePath());
			parameters.put("propertyName", "Response");
			CodeGenUtil.processTemplate(
					CIXS_JAXWS_GENERATOR_NAME,
					OPERATION_HOLDER_VLC_TEMPLATE,
					"cixsOperation", operation,
					parameters,
					targetFile);
		}
	}

	/**
	 * @return the Jaxws service 
	 */
	public final CixsJaxwsService getCixsJaxwsService() {
		return mCixsJaxwsService;
	}

	/**
	 * @param cixsJaxwsComponent the Jaxws service to set
	 */
	public final void setCixsJaxwsService(
			final CixsJaxwsService cixsJaxwsComponent) {
		mCixsJaxwsService = cixsJaxwsComponent;
	}

	/**
	 * @param cixsJaxwsComponent the Jaxws service to set
	 */
	public final void add(final CixsJaxwsService cixsJaxwsComponent) {
		mCixsJaxwsService = cixsJaxwsComponent;
	}

	/**
	 * @param cixsJaxwsComponent the Jaxws service to set
	 */
	public final void addCixsJaxwsService(
			final CixsJaxwsService cixsJaxwsComponent) {
		mCixsJaxwsService = cixsJaxwsComponent;
	}

	/**
	 * @return the target source directory
	 */
	public final String getTargetSrcDir() {
		return mTargetSrcDir;
	}

	/**
	 * @param targetSrcDir the target source directory to set
	 */
	public final void setTargetSrcDir(final String targetSrcDir) {
		mTargetSrcDir = targetSrcDir;
	}

	/**
	 * @return the location for web deployment descriptors
	 */
	public final String getTargetWDDDir() {
		return mTargetWDDDir;
	}

	/**
	 * @param targetWDDDir the location for web deployment descriptors
	 *  to set
	 */
	public final void setTargetWDDDir(
			final String targetWDDDir) {
		mTargetWDDDir = targetWDDDir;
	}

	/**
	 * @return the location for ant deployment script
	 */
	public final String getTargetAntDir() {
		return mTargetAntDir;
	}

	/**
	 * @param targetAntDir the location for ant deployment script to set
	 */
	public final void setTargetAntDir(final String targetAntDir) {
		mTargetAntDir = targetAntDir;
	}

	/**
	 * @return the jaxws endpoint binaries
	 */
	public final String getCixsBinDir() {
		return mCixsBinDir;
	}

	/**
	 * @param cixsBinDir the jaxws endpoint binaries to set
	 */
	public final void setCixsBinDir(final String cixsBinDir) {
		mCixsBinDir = cixsBinDir;
	}

	/**
	 * @return the jaxb binaries location
	 */
	public final String getJaxbBinDir() {
		return mJaxbBinDir;
	}

	/**
	 * @param jaxbBinDir the jaxb binaries location to set
	 */
	public final void setJaxbBinDir(final String jaxbBinDir) {
		mJaxbBinDir = jaxbBinDir;
	}

	/**
	 * @return the coxb binaries location
	 */
	public final String getCoxbBinDir() {
		return mCoxbBinDir;
	}

	/**
	 * @param coxbBinDir the coxb binaries location to set
	 */
	public final void setCoxbBinDir(final String coxbBinDir) {
		mCoxbBinDir = coxbBinDir;
	}

	/**
	 * @return the target war files location
	 */
	public final String getTargetWarDir() {
		return mTargetWarDir;
	}

	/**
	 * @param targetWarDir the target war files location to set
	 */
	public final void setTargetWarDir(final String targetWarDir) {
		mTargetWarDir = targetWarDir;
	}

	/**
	 * @return custom binaries location
	 */
	public final String getCustBinDir() {
		return mCustBinDir;
	}

	/**
	 * @param custBinDir the custom binaries location to set
	 */
	public final void setCustBinDir(final String custBinDir) {
		mCustBinDir = custBinDir;
	}

	/**
	 * @return the target properties files location
	 */
	public final String getTargetPropDir() {
		return mTargetPropDir;
	}

	/**
	 * @param targetPropDir the target properties files location to set
	 */
	public final void setTargetPropDir(final String targetPropDir) {
		mTargetPropDir = targetPropDir;
	}


}

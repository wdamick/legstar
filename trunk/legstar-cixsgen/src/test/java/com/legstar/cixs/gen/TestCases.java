package com.legstar.cixs.gen;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.gen.model.CixsStructure;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.cixs.model.util.ModelUtil;

/**
 * Produces models to use for generation testing.
 */
public class TestCases {
	
	private final static String CIXS_PACKAGE_PREFIX = "com.legstar.test.cixs.";
	
	private final static String COXB_PACKAGE_PREFIX = "com.legstar.test.coxb.";

	private final static String NAMESPACE_PREFIX = "http://cixs.test.legstar.com/";
	
	/** A case with a regular commarea. */
	public static CixsJaxwsService getLsfileae() {
		return getCommareaService("lsfileae");
	}
	
	/** A case with different input and output commareas. */
	public static CixsJaxwsService getLsfileal() {
		return getCommareaService("lsfileal", "RequestParmsType", "ReplyDataType");
	}

	/** A case with with multiple input and output containers. */
	public static CixsJaxwsService getLsfileac() {
		Map <String, String> inputContainers = new HashMap <String, String>();
		inputContainers.put("QueryDataType", "QueryData");
		inputContainers.put("QueryLimitType", "QueryLimit");
		Map <String, String> outputContainers = new HashMap <String, String>();
		outputContainers.put("ReplyDataType", "ReplyData");
		outputContainers.put("ReplyStatusType", "ReplyStatus");
		return getContainerService("lsfileac", inputContainers, outputContainers);
	}
	
	/** A case with multiple operations. */
	public static CixsJaxwsService getLsfileax() {
		CixsJaxwsService service = getNewService("lsfileax");
		service.getCixsOperations().add(
				getNewCommareaOperation("lsfileae", "DfhcommareaType", "DfhcommareaType"));
		Map <String, String> inputContainers = new HashMap <String, String>();
		inputContainers.put("QueryDataType", "QueryData");
		inputContainers.put("QueryLimitType", "QueryLimit");
		Map <String, String> outputContainers = new HashMap <String, String>();
		outputContainers.put("ReplyDataType", "ReplyData");
		outputContainers.put("ReplyStatusType", "ReplyStatus");
		service.getCixsOperations().add(getNewContainerOperation("lsfileac", inputContainers, outputContainers));
		return service;
	}

	/** A case where the operation has a different namespace/package than the service. */
	public static CixsJaxwsService getLsfilean() {
		CixsJaxwsService service = getNewService("lsfilean");
		CixsOperation operation = getNewCommareaOperation("lsfileae", "DfhcommareaType", "DfhcommareaType");
		operation.setNamespace(NAMESPACE_PREFIX + "oper/" + "lsfilean");
		operation.setPackageName(CIXS_PACKAGE_PREFIX + "oper." + "lsfilean");
		service.getCixsOperations().add(operation);
		return service;
	}
	
	/** A case where there is no package names. */
	public static CixsJaxwsService getLsfileap() {
		CixsJaxwsService service = getNewService("lsfileap");
		CixsOperation operation = getNewCommareaOperation("lsfileae", "DfhcommareaType", "DfhcommareaType");
		service.setPackageName(null);
		service.getCixsOperations().add(operation);
		return service;
	}

	/**
	 * Helper function to setup a Service in case of a single, commarea-driven
	 * operation with identical input and output layouts.
	 *  */
	public static CixsJaxwsService getCommareaService(
			String name) {
		return getCommareaService(name, "DfhcommareaType", "DfhcommareaType");
	}

	/**
	 * Helper function to setup a Service in case of a single, commarea-driven
	 * operation with different input and output layouts.
	 *  */
	private static CixsJaxwsService getCommareaService(
			String name, String inputJaxbType, String outputJaxbType) {
		CixsJaxwsService service = getNewService(name);
		service.getCixsOperations().add(getNewCommareaOperation(name, inputJaxbType, outputJaxbType));
		return service;
	}
	
	/** 
	 * Helper function to setup a Service in case of a container-driven
	 * operation.
	 *  */
	private static  CixsJaxwsService getContainerService(
			String name,
			Map <String, String> inputContainers,
			Map <String, String> outputContainers) {

		CixsJaxwsService service = getNewService(name);
		CixsOperation operation = getNewContainerOperation(name, inputContainers, outputContainers);
		service.getCixsOperations().add(operation);
		
		return service;
	}
	
	private static CixsJaxwsService getNewService(String name) {
		CixsJaxwsService serviceModel = new CixsJaxwsService();
		serviceModel.setPackageName(CIXS_PACKAGE_PREFIX + name);
		serviceModel.setImplementationClassName(
				ModelUtil.classNormalize(name) + "Impl");
		serviceModel.setInterfaceClassName(
				ModelUtil.classNormalize(name));
		serviceModel.setName(name);
		serviceModel.setTargetNamespace(NAMESPACE_PREFIX + name);
		return serviceModel;
	}
	
	private static CixsOperation getNewCommareaOperation(String name, String inputJaxbType, String outputJaxbType) {
		CixsOperation operation = new CixsOperation();
		operation.setInput(new ArrayList < CixsStructure >());
		operation.setOutput(new ArrayList < CixsStructure >());
		CixsStructure inStruct = new CixsStructure();
		CixsStructure outStruct = new CixsStructure();
		
		operation.setName(name);
		operation.setCicsProgramName(name.toUpperCase());
		inStruct.setJaxbType(inputJaxbType);
		inStruct.setJaxbPackageName(COXB_PACKAGE_PREFIX + name);
		outStruct.setJaxbType(outputJaxbType);
		outStruct.setJaxbPackageName(inStruct.getJaxbPackageName());
		
		operation.getInput().add(inStruct);
		operation.getOutput().add(outStruct);
		
		return operation;
	}

	private static CixsOperation getNewContainerOperation(
			String name,
			Map <String, String> inputContainers,
			Map <String, String> outputContainers) {
		CixsOperation operation = new CixsOperation();
		operation.setInput(new ArrayList < CixsStructure >());
		operation.setOutput(new ArrayList < CixsStructure >());
		
		operation.setName(name);
		operation.setCicsProgramName(name.toUpperCase());
		operation.setCicsChannel(name.toUpperCase() + "-CHANNEL");

		for(String jaxbType : inputContainers.keySet()) {
			CixsStructure inStruct = new CixsStructure();
			inStruct.setJaxbType(jaxbType);
			inStruct.setJaxbPackageName(COXB_PACKAGE_PREFIX + name);
			inStruct.setCicsContainer(inputContainers.get(jaxbType));
			operation.getInput().add(inStruct);
		}
		
		for(String jaxbType : outputContainers.keySet()) {
			CixsStructure outStruct = new CixsStructure();
			outStruct.setJaxbType(jaxbType);
			outStruct.setJaxbPackageName(COXB_PACKAGE_PREFIX + name);
			outStruct.setCicsContainer(outputContainers.get(jaxbType));
			operation.getOutput().add(outStruct);
		}
		
		return operation;
	}

}

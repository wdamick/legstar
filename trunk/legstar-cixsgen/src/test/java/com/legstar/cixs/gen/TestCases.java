package com.legstar.cixs.gen;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.gen.model.CixsStructure;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.codegen.CodeGenUtil;

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
				getNewCommareaOperation("lsfileax", "lsfileae", "DfhcommareaType", "DfhcommareaType"));
		Map <String, String> inputContainers = new HashMap <String, String>();
		inputContainers.put("QueryDataType", "QueryData");
		inputContainers.put("QueryLimitType", "QueryLimit");
		Map <String, String> outputContainers = new HashMap <String, String>();
		outputContainers.put("ReplyDataType", "ReplyData");
		outputContainers.put("ReplyStatusType", "ReplyStatus");
		service.getCixsOperations().add(getNewContainerOperation("lsfileax", "lsfileac", inputContainers, outputContainers));
		return service;
	}

	/** A case where the operation has a different namespace/package than the service. */
	public static CixsJaxwsService getLsfilean() {
		CixsJaxwsService service = getNewService("lsfilean");
		CixsOperation operation = getNewCommareaOperation("lsfilean", "lsfileae", "DfhcommareaType", "DfhcommareaType");
		operation.setNamespace(NAMESPACE_PREFIX + "oper/" + "lsfilean");
		operation.setPackageName(CIXS_PACKAGE_PREFIX + "oper." + "lsfilean");
		service.getCixsOperations().add(operation);
		return service;
	}
	
	/** A case where there is no operation package names. */
	public static CixsJaxwsService getLsfileap() {
		CixsJaxwsService service = getNewService("lsfileap");
		CixsOperation operation = getNewCommareaOperation("lsfileap", "lsfileae", "DfhcommareaType", "DfhcommareaType");
		operation.setPackageName(null);
		service.setPackageName(null);
		service.getCixsOperations().add(operation);
		return service;
	}

	/** A case with with single input and output containers. */
	public static CixsJaxwsService getLsfileaq() {
		CixsJaxwsService service = getNewService("lsfileaq");
		Map <String, String> inputContainers = new HashMap <String, String>();
		inputContainers.put("QueryDataType", "QueryData");
		Map <String, String> outputContainers = new HashMap <String, String>();
		outputContainers.put("ReplyDataType", "ReplyData");
		service.getCixsOperations().add(getNewContainerOperation("lsfileaq", "lsfileac", inputContainers, outputContainers));
		return service;
	}
	
	public static CixsJaxwsService getJvmquery() throws Exception {
		CixsJaxwsService service = getNewService("jvmquery");
		service.addCixsOperation(getJvmqueryOperation());
		service.setServiceURI("http://192.168.0.5/");
		service.setServiceUserId("alice");
		service.setServicePassword("inwonderland");
		return service;
	}
	
	public static CixsJaxwsService getCutureInfoModel() throws Exception {
		CixsJaxwsService service = getNewService("cultureinfo");
		service.addCixsOperation(getCutureInfoOperation());
		service.setTargetNamespace("http://cultureinfo.cases.test.xsdc.legstar.com/");
		service.setWsdlUrl("http://localhost:8080/jaxws-cultureinfo/getinfo?wsdl");
		service.setWsdlServiceName("CultureInfoImplService");
		service.setWsdlPortName("CultureInfoImplPort");
		service.setServiceURI("http://localhost:8080/jaxws-cultureinfo/getinfo");
		return service;
	}
	
	public static CixsOperation getCutureInfoOperation() {
		return getNewCommareaOperation("cultureinfo", "cultureinfo", "GetInfoType", "GetInfoResponseType");
	}
	
	public static CixsOperation getJvmqueryOperation() {
		return getNewCommareaOperation("jvmquery", "jvmquery", "JvmQueryRequestType", "JvmQueryReplyType");
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
		service.getCixsOperations().add(getNewCommareaOperation(name, name, inputJaxbType, outputJaxbType));
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
		CixsOperation operation = getNewContainerOperation(name, name, inputContainers, outputContainers);
		service.getCixsOperations().add(operation);
		
		return service;
	}
	
	private static CixsJaxwsService getNewService(String name) {
		CixsJaxwsService model = new CixsJaxwsService();
		model.setPackageName(CIXS_PACKAGE_PREFIX + name);
		model.setImplementationClassName(
				CodeGenUtil.classNormalize(name) + "Impl");
		model.setInterfaceClassName(
				CodeGenUtil.classNormalize(name));
		model.setName(name);
		model.setTargetNamespace(NAMESPACE_PREFIX + name);
		return model;
	}
	
	private static CixsOperation getNewCommareaOperation(
			String serviceName,
			String operationName,
			String inputJaxbType,
			String outputJaxbType) {
		CixsOperation operation = new CixsOperation();
		operation.setInput(new ArrayList < CixsStructure >());
		operation.setOutput(new ArrayList < CixsStructure >());
		CixsStructure inStruct = new CixsStructure();
		CixsStructure outStruct = new CixsStructure();
		
		operation.setName(operationName);
		operation.setCicsProgramName(operationName.toUpperCase());
		operation.setNamespace(NAMESPACE_PREFIX + serviceName);
		operation.setPackageName(CIXS_PACKAGE_PREFIX + serviceName);
		inStruct.setJaxbType(inputJaxbType);
		inStruct.setJaxbPackageName(COXB_PACKAGE_PREFIX + operationName);
		outStruct.setJaxbType(outputJaxbType);
		outStruct.setJaxbPackageName(inStruct.getJaxbPackageName());
		
		operation.getInput().add(inStruct);
		operation.getOutput().add(outStruct);
		
		return operation;
	}

	private static CixsOperation getNewContainerOperation(
			String serviceName,
			String operationName,
			Map <String, String> inputContainers,
			Map <String, String> outputContainers) {
		CixsOperation operation = new CixsOperation();
		operation.setInput(new ArrayList < CixsStructure >());
		operation.setOutput(new ArrayList < CixsStructure >());
		
		operation.setName(operationName);
		operation.setNamespace(NAMESPACE_PREFIX + serviceName);
		operation.setPackageName(CIXS_PACKAGE_PREFIX + serviceName);
		operation.setCicsProgramName(operationName.toUpperCase());
		operation.setCicsChannel(operationName.toUpperCase() + "-CHANNEL");

		for(String jaxbType : inputContainers.keySet()) {
			CixsStructure inStruct = new CixsStructure();
			inStruct.setJaxbType(jaxbType);
			inStruct.setJaxbPackageName(COXB_PACKAGE_PREFIX + operationName);
			inStruct.setCicsContainer(inputContainers.get(jaxbType));
			operation.getInput().add(inStruct);
		}
		
		for(String jaxbType : outputContainers.keySet()) {
			CixsStructure outStruct = new CixsStructure();
			outStruct.setJaxbType(jaxbType);
			outStruct.setJaxbPackageName(COXB_PACKAGE_PREFIX + operationName);
			outStruct.setCicsContainer(outputContainers.get(jaxbType));
			operation.getOutput().add(outStruct);
		}
		
		return operation;
	}

}

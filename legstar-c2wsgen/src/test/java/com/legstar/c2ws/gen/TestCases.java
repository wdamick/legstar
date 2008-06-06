package com.legstar.c2ws.gen;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.gen.model.CixsStructure;

/**
 * Produces models to use for generation testing.
 */
public class TestCases {
	
	private final static String CIXS_PACKAGE_PREFIX = "com.legstar.test.cixs.";
	
	private final static String COXB_PACKAGE_PREFIX = "com.legstar.test.coxb.";

	private final static String NAMESPACE_PREFIX = "http://cixs.test.legstar.com/";
	
	public static CixsOperation getLsfileaeOperation() {
		return getNewCommareaOperation("lsfileae", "lsfileae", "DfhcommareaType", "DfhcommareaType");
	}
	
	public static CixsOperation getJvmqueryOperation() {
		return getNewCommareaOperation("jvmquery", "jvmquery", "JvmQueryRequest", "JvmQueryReply");
	}
	
	public static CixsOperation getLsfileacOperation() {
		Map <String, String> inputContainers = new HashMap <String, String>();
		inputContainers.put("QueryDataType", "QueryData");
		inputContainers.put("QueryLimitType", "QueryLimit");
		Map <String, String> outputContainers = new HashMap <String, String>();
		outputContainers.put("ReplyDataType", "ReplyData");
		outputContainers.put("ReplyStatusType", "ReplyStatus");
		return getNewContainerOperation("lsfileac", "lsfileac", inputContainers, outputContainers);
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

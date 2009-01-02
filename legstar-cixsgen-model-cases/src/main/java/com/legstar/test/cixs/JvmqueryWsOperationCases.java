package com.legstar.test.cixs;

import java.util.ArrayList;
import java.util.List;

import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.gen.model.CixsStructure;

/**
 * Mapping cases for Jvmquery when it is exposed as a Web Service.
 *
 */
public class JvmqueryWsOperationCases extends AbstractOperationCases {

    /**
     * @param serviceName the service name
     * @param operationPackageName the operation classes package name
     * @param operationNamespace the operation namespace
     * @return an operation corresponding to a Web Service operation.
     */
    public static CixsOperation getOperation(
            final String serviceName,
            final String operationPackageName,
            final String operationNamespace) {
        CixsOperation cixsOperation = new CixsOperation();
        cixsOperation.setName("queryJvm");
        cixsOperation.setCicsProgramName("JVMQUERY");
        cixsOperation.setPackageName(operationPackageName);
        cixsOperation.setNamespace(operationNamespace);

        List < CixsStructure > inStructures = new ArrayList < CixsStructure >();
        CixsStructure inStructure = new CixsStructure();
        inStructure.setJaxbPackageName(JAXB_PKG_PREFIX + ".ws.jvmquery");
        inStructure.setJaxbType("QueryJvm");
        inStructures.add(inStructure);
        cixsOperation.setInput(inStructures);

        List < CixsStructure > outStructures = new ArrayList < CixsStructure >();
        CixsStructure outStructure = new CixsStructure();
        outStructure.setJaxbPackageName(JAXB_PKG_PREFIX + ".ws.jvmquery");
        outStructure.setJaxbType("QueryJvmResponse");
        outStructures.add(outStructure);
        cixsOperation.setOutput(outStructures);

        return cixsOperation;
    }

}

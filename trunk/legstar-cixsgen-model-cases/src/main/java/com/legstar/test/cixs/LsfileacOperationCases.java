package com.legstar.test.cixs;

import java.util.ArrayList;
import java.util.List;

import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.gen.model.CixsStructure;

/**
 * Mapping cases for LSFILEAC. A container-driven program with 2 input
 * containers and 2 output containers.
 *
 */
public class LsfileacOperationCases extends AbstractOperationCases {

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
        cixsOperation.setName("lsfileac");
        cixsOperation.setCicsProgramName("LSFILEAC");
        cixsOperation.setPackageName(operationPackageName);
        cixsOperation.setNamespace(operationNamespace);
        cixsOperation.setCicsChannel("LSFILEAC-CHANNEL");
        cixsOperation.setFaultType("LsfileacException");

        List < CixsStructure > inStructures = new ArrayList < CixsStructure >();
        CixsStructure inStructure1 = new CixsStructure();
        inStructure1.setJaxbPackageName(JAXB_PKG_PREFIX + ".lsfileac");
        inStructure1.setJaxbType("QueryData");
        inStructure1.setCicsContainer("QueryData");
        inStructures.add(inStructure1);

        CixsStructure inStructure2 = new CixsStructure();
        inStructure2.setJaxbPackageName(JAXB_PKG_PREFIX + ".lsfileac");
        inStructure2.setJaxbType("QueryLimit");
        inStructure2.setCicsContainer("QueryLimit");
        inStructures.add(inStructure2);

        cixsOperation.setInput(inStructures);

        List < CixsStructure > outStructures = new ArrayList < CixsStructure >();
        CixsStructure outStructure1 = new CixsStructure();
        outStructure1.setJaxbPackageName(JAXB_PKG_PREFIX + ".lsfileac");
        outStructure1.setJaxbType("ReplyData");
        outStructure1.setCicsContainer("ReplyData");
        outStructures.add(outStructure1);

        CixsStructure outStructure2 = new CixsStructure();
        outStructure2.setJaxbPackageName(JAXB_PKG_PREFIX + ".lsfileac");
        outStructure2.setJaxbType("ReplyStatus");
        outStructure2.setCicsContainer("ReplyStatus");
        outStructures.add(outStructure2);

        cixsOperation.setOutput(outStructures);

        return cixsOperation;
    }

}

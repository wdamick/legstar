package com.legstar.test.cixs;

import java.util.ArrayList;
import java.util.List;

import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.gen.model.CixsStructure;

/**
 * Mapping cases for LSFILEAL. A commarea-driven program where the input
 * commarea layout is different from the output commarea layout.
 *
 */
public class LsfilealOperationCases extends AbstractOperationCases {

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
        cixsOperation.setName("lsfileal");
        cixsOperation.setCicsProgramName("LSFILEAL");
        cixsOperation.setPackageName(operationPackageName);
        cixsOperation.setNamespace(operationNamespace);
        cixsOperation.setFaultType("LsfilealException");

        List < CixsStructure > inStructures = new ArrayList < CixsStructure >();
        CixsStructure inStructure = new CixsStructure();
        inStructure.setJaxbPackageName(JAXB_PKG_PREFIX + ".lsfileal");
        inStructure.setJaxbType("RequestParms");
        inStructures.add(inStructure);
        cixsOperation.setInput(inStructures);

        List < CixsStructure > outStructures = new ArrayList < CixsStructure >();
        CixsStructure outStructure = new CixsStructure();
        outStructure.setJaxbPackageName(JAXB_PKG_PREFIX + ".lsfileal");
        outStructure.setJaxbType("ReplyData");
        outStructures.add(outStructure);
        cixsOperation.setOutput(outStructures);

        return cixsOperation;
    }

}

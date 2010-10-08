/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.test.cixs;

import java.util.ArrayList;
import java.util.List;

import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.gen.model.CixsStructure;

/**
 * Mapping cases for DPLARCHT. A complex commarea-driven program.
 *
 */
public class DplarchtOperationCases extends AbstractOperationCases {

    /**
     * @param serviceName the service name
     * @param operationPackageName the operation classes package name
     * @return an operation corresponding to a Web Service operation.
     */
    public static CixsOperation getOperation(
            final String serviceName,
            final String operationPackageName) {
        CixsOperation cixsOperation = new CixsOperation();
        cixsOperation.setName("dplarcht");
        cixsOperation.setCicsProgramName("DPLARCHT");
        cixsOperation.setPackageName(operationPackageName);
        cixsOperation.setFaultType("DplarchtException");

        List < CixsStructure > inStructures = new ArrayList < CixsStructure >();
        CixsStructure inStructure = new CixsStructure();
        inStructure.setJaxbPackageName(JAXB_PKG_PREFIX + ".dplarcht");
        inStructure.setJaxbType("Dfhcommarea");
        inStructures.add(inStructure);
        cixsOperation.setInput(inStructures);

        List < CixsStructure > outStructures = new ArrayList < CixsStructure >();
        CixsStructure outStructure = new CixsStructure();
        outStructure.setJaxbPackageName(JAXB_PKG_PREFIX + ".dplarcht");
        outStructure.setCustPackageName(CUST_PKG_PREFIX + ".dplarcht");
        outStructure.setJaxbType("Dfhcommarea");
        outStructures.add(outStructure);
        cixsOperation.setOutput(outStructures);

        return cixsOperation;
    }

}

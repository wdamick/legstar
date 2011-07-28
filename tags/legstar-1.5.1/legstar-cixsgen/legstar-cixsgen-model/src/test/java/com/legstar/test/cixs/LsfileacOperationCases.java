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

import com.legstar.cixs.gen.model.CixsOperation;

/**
 * Mapping cases for LSFILEAC. A container-driven program with 2 input
 * containers and 2 output containers.
 * 
 */
public class LsfileacOperationCases extends AbstractOperationCases {

    /**
     * @param serviceName the service name
     * @param operationPackageName the operation classes package name
     * @return an operation corresponding to a Web Service operation.
     */
    public static CixsOperation getOperation(final String serviceName,
            final String operationPackageName) {
        CixsOperation cixsOperation = new CixsOperation();
        cixsOperation.setName("lsfileac");
        cixsOperation.setCicsProgramName("LSFILEAC");
        cixsOperation.setPackageName(operationPackageName);
        cixsOperation.setCicsChannel("LSFILEAC-CHANNEL");
        cixsOperation.setFaultType("LsfileacException");

        cixsOperation.addInput(createCixsStructure(serviceName, "QueryData",
                "QueryData", false));
        cixsOperation.addInput(createCixsStructure(serviceName, "QueryLimit",
                "QueryLimit", false));
        cixsOperation.addOutput(createCixsStructure(serviceName, "ReplyData",
                "ReplyData", false));
        cixsOperation.addOutput(createCixsStructure(serviceName, "ReplyStatus",
                "ReplyStatus", false));

        return cixsOperation;
    }

}

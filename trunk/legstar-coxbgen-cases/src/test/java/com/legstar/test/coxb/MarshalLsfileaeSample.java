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
package com.legstar.test.coxb;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.impl.visitor.CobolMarshalVisitor;
import com.legstar.test.coxb.lsfileae.ComPersonal;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;
import com.legstar.test.coxb.lsfileae.bind.DfhcommareaBinding;

/**
 * Sample demonstrating how to use a COXB object to marshal java data to
 * z/os data.
 *
 */
public class MarshalLsfileaeSample {

    /** Utility class. */
    private MarshalLsfileaeSample() {
        
    }
    /**
     * The main function does not take any argument.
     * @param args
     */
    /**
     * Marshal host data and test java data object result.
     * @param args not used
     */
    public static void main(final String[] args) {
        // Create a JAXB object and set its properties
        Dfhcommarea jaxbObject = getJaxbObject();
        // Create a COXB object wrapping the JAXB object
        DfhcommareaBinding coxbObject = new DfhcommareaBinding(jaxbObject);
        try {
            // Create a buffer to receive the host data
            byte[] hostBytes = new byte[coxbObject.getByteLength()];
            // Create a marshaler using the default cobol conversion parameters
            CobolMarshalVisitor mv = new CobolMarshalVisitor(
                    hostBytes, 0, new CobolSimpleConverters(new CobolContext()));
            // Marshal the java data to a z/os buffer
            coxbObject.accept(mv);
            // Print the result
            System.out.println("z/os data=" + HostData.toHexString(hostBytes));

        } catch (HostException e) {
            e.printStackTrace();
        }
    }

    /**
     * This method creates an instance of a JAXB object and sets its properties.
     * @return a JAXB object
     */
    private static Dfhcommarea getJaxbObject() {
        Dfhcommarea dfhcommarea = new Dfhcommarea();
        dfhcommarea.setComNumber(100);
        dfhcommarea.setComDate("100458");
        dfhcommarea.setComAmount("00100.35");
        dfhcommarea.setComComment("A VOIR");
        ComPersonal personal = new ComPersonal();
        personal.setComName("TOTO");
        personal.setComAddress("LABAS STREET");
        personal.setComPhone("88993314");
        dfhcommarea.setComPersonal(personal);
        return dfhcommarea;
    }

}

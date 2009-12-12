/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.test.coxb;



import java.io.StringWriter;

import com.legstar.coxb.host.HostData;
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.test.coxb.lsfileae.bind.DfhcommareaHostToJavaTransformer;
import com.legstar.test.coxb.lsfileae.bind.DfhcommareaTransformers;
import com.legstar.test.coxb.lsfileae.bind.DfhcommareaXmlTransformers;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;

import junit.framework.TestCase;

/**
 * Unmarshal lsfileae.
 *
 */
public class UnmarshalLsfileaeTest extends TestCase {

    /**
     * Unmarshal host data and test java data object result.
     * @throws Exception if marshaling fails
     */
    public void testLsfileae() throws Exception {

        String hexString = LsfileaeCases.getHostBytesHex();
        byte[] hostBytes = HostData.toByteArray(hexString);
        Dfhcommarea dfhcommarea = (Dfhcommarea) Util.unmarshal(hostBytes, "lsfileae");
        LsfileaeCases.checkJavaObject(dfhcommarea);
    }
    /**
     * Transform host data and test java data object result.
     * @throws HostTransformException if transforming fails
     */
    public void testHostToJavaTransformer() throws HostTransformException {

        DfhcommareaHostToJavaTransformer transformer = new DfhcommareaHostToJavaTransformer();
        Dfhcommarea dfhcommarea = transformer.transform(HostData.toByteArray(LsfileaeCases.getHostBytesHex()));
        LsfileaeCases.checkJavaObject(dfhcommarea);
    }
    
    /**
     * Test the sample code shown in documentation.
     * @throws HostTransformException if transforming fails
     */
    public void testHostToJavaTransformerDoc() throws HostTransformException {

        hostToJavaTransform(HostData.toByteArray(LsfileaeCases.getHostBytesHex()));
        hostToXmlTransform(HostData.toByteArray(LsfileaeCases.getHostBytesHex()));
    }
    /**
     * Transform host data and test java data object result.
     * @param hostBytes a byte array holding the mainframe payload
     * @throws HostTransformException if transforming fails
     */
    public void hostToJavaTransform(final byte[] hostBytes) throws HostTransformException {

        DfhcommareaTransformers transformers = new DfhcommareaTransformers();
        Dfhcommarea dfhcommarea = transformers.toJava(hostBytes);
        System.out.println(dfhcommarea.getComNumber());
        System.out.println(dfhcommarea.getComPersonal().getComName());
        System.out.println(dfhcommarea.getComPersonal().getComAddress());
        System.out.println(dfhcommarea.getComPersonal().getComPhone());
        System.out.println(dfhcommarea.getComDate());
        System.out.println(dfhcommarea.getComAmount());
        System.out.println(dfhcommarea.getComComment());
    }

    /**
     * Transform host data and test XML result.
     * @param hostBytes a byte array holding the mainframe payload
     * @throws HostTransformException if transforming fails
     */
    public void hostToXmlTransform(final byte[] hostBytes) throws HostTransformException {

        DfhcommareaXmlTransformers transformers = new DfhcommareaXmlTransformers();
        StringWriter writer = new StringWriter();
        transformers.toXml(hostBytes, writer);
        System.out.println(writer.toString());
    }
}

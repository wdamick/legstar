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



import java.io.StringWriter;

import com.legstar.coxb.host.HostData;
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.test.coxb.lsfileaq.bind.DfhcommareaTransformers;
import com.legstar.test.coxb.lsfileaq.bind.DfhcommareaXmlTransformers;

import junit.framework.TestCase;

/**
 * Unmarshal lsfileaq.
 *
 */
public class UnmarshalLsfileaqTest extends TestCase {

    /** The generated transformers.*/
    private DfhcommareaTransformers transformers;

    /** The generated XML transformers.*/
    private DfhcommareaXmlTransformers xmlTransformers;

    /** {@inheritDoc}*/
    public void setUp() throws Exception {
        xmlTransformers = new DfhcommareaXmlTransformers();
        transformers = new DfhcommareaTransformers();
    }

    /**
     * Transform host data and test java data object result.
     * @throws HostTransformException if transforming fails
     */
    public void testHostToJavaTransformer() throws HostTransformException {
        LsfileaqCases.checkJavaObjectReply5(
                transformers.toJava(
                        HostData.toByteArray(LsfileaqCases.getHostBytesHexRequestReply5())));
    }

    /**
     * Transform host data and test XML result.
     * @throws HostTransformException if transforming fails
     */
    public void testHostToXmlTransformer() throws HostTransformException {
        StringWriter sw = new StringWriter();
        xmlTransformers.toXml(
                HostData.toByteArray(LsfileaqCases.getHostBytesHexRequestReply5()), sw);
        assertEquals(LsfileaqCases.getXmlRequestReply5(), sw.toString());
    }
}

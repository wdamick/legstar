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

import java.io.StringReader;

import javax.xml.transform.stream.StreamSource;

import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.lsfileaq.bind.DfhcommareaTransformers;
import com.legstar.test.coxb.lsfileaq.bind.DfhcommareaXmlTransformers;

import junit.framework.TestCase;

/**
 * Marshal lsfileaq.
 *
 */
public class MarshalLsfileaqTest extends TestCase {
	
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
     * Transform java data object with default values set.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformerDefaults() throws Exception {
         assertEquals(LsfileaqCases.getHostBytesHexRequestDefaults(),
                HostData.toHexString(transformers.toHost(LsfileaqCases.getJavaObjectRequestDefaults())));
    }

    /**
     * Transform java data object with all values set.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformerValued() throws Exception {
        assertEquals(LsfileaqCases.getHostBytesHexRequest5(),
                HostData.toHexString(transformers.toHost(LsfileaqCases.getJavaObjectRequest5())));
    }

    /**
     * Transform java data object with complete request/reply set.
     * @throws Exception if transforming fails
     */
    public void testJavaToHostTransformerComplete() throws Exception {
        assertEquals(LsfileaqCases.getHostBytesHexRequestReply5(),
                HostData.toHexString(transformers.toHost(LsfileaqCases.getJavaObjectRequestReply5())));
    }
    
    /**
     * Transform XML with complete request/reply set.
     * @throws Exception if transforming fails
     */
    public void testXmlTransformerComplete() throws Exception {
        assertEquals(LsfileaqCases.getHostBytesHexRequestReply5(),
                HostData.toHexString(
                		xmlTransformers.toHost(
                				new StreamSource(new StringReader(
                						LsfileaqCases.getXmlRequestReply5()))
                				)));
    }
}

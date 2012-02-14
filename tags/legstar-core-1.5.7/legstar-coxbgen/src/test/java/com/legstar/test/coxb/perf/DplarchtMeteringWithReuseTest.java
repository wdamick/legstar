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
package com.legstar.test.coxb.perf;

import junit.framework.TestCase;

import com.legstar.coxb.host.HostData;
import com.legstar.coxb.transform.HostTransformException;
import com.legstar.test.coxb.DplarchtCases;
import com.legstar.test.coxb.dplarcht.Dfhcommarea;
import com.legstar.test.coxb.dplarcht.bind.DfhcommareaTransformers;


/**
 * This class is useful for performance testing with JMeter.
 * It simulates 3 different payload sizes by varying the number of items in
 * a variable size array.
 * Reusing the same transformet set.
 */
public class DplarchtMeteringWithReuseTest extends TestCase {
    
    /** Number of files in variable size array. */
    private int mFiles;
    
    /** Host data for the given number of files. */
    private byte[] mHostBytes;
    
    /** The transformer set.*/
    private DfhcommareaTransformers _transformers = new DfhcommareaTransformers();
    
    /**
     * By default, considers empty variable size array.
     */
    public DplarchtMeteringWithReuseTest() {
        this("0");
    }
    
    /**
     * @param label JMeter passes a label that we use as a parameter for this test 
     */
    public DplarchtMeteringWithReuseTest(final String label) {
        super(label);
        try {
            mFiles = Integer.parseInt(label);
        } catch (NumberFormatException e) {
            mFiles = 0;
        }
        mHostBytes = HostData.toByteArray(DplarchtCases.getHostBytesHexFiles(mFiles));
    }
    
    /**
     * DPLARCHT from Host to Java.
     */
    public void testHostToJava() {
        try {
            Dfhcommarea dfhcommarea = (Dfhcommarea) _transformers.toJava(
                    getHostBytes());
            DplarchtCases.checkJavaObjectFiles(getFiles(), dfhcommarea);
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }

    /**
     * DPLARCHT from Java to Host.
     */
    public void testJavaToHost() {
        try {
            byte[] hostBytes = _transformers.toHost(
                    DplarchtCases.getJavaObjectFiles(getFiles()));
            assertEquals(getHostBytes().length, hostBytes.length);
        } catch (HostTransformException e) {
            fail(e.getMessage());
        }
    }

    /**
     * @return the number of files in variable size array
     */
    public int getFiles() {
        return mFiles;
    }

    /**
     * @return the host data for the given number of files
     */
    public byte[] getHostBytes() {
        return mHostBytes;
    }
}

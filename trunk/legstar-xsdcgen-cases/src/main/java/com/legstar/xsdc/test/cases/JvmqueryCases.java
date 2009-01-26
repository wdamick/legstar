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
package com.legstar.xsdc.test.cases;

import junit.framework.TestCase;

import com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply;
import com.legstar.xsdc.test.cases.jvmquery.JVMQueryRequest;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public final class JvmqueryCases extends TestCase {

    /** Expected raw mainframe response sample. */
    public static final String JVMQUERYREPLY_HOST_BYTES =
        /* 0 0 0 2 F r a n c e - - - - - - - - - - - - - - - - - - - - - -*/
        "00000002c6998195838540404040404040404040404040404040404040404040"
        /* - - - - € - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
        + "404040409f404040404040404040404040404040404040404040404040404040"
        /* - - - - D : \ L e g s e m \ L e g s t a r \ j b o s s \ m l i t*/
        + "40404040c47ae0d38587a28594e0d38587a2a38199e0918296a2a2e0949389a3"
        /* t l e \ C : \ P r o g r a m - F i l e s \ J a v a \ j d k 1 . 6*/
        + "a39385e0c37ae0d799968799819440c6899385a2e0d181a581e0918492f14bf6"
        /* . 0 - - v e n d r e d i - 1 0 - o c t o b r e - 2 0 0 8 - 1 4 -*/
        + "4bf04040a58595849985848940f1f0409683a39682998540f2f0f0f840f1f440"
        /* h - 2 8 f r a n ç a i s - - - - - - - - - - - - - - - - - - - -*/
        + "8840f2f886998195488189a24040404040404040404040404040404040404040"
        /* - - - - */
        + "40404040";

    /** Utility class. */
    private JvmqueryCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static JVMQueryReply getJavaObjectReplyFrance() {
        JVMQueryReply jvmQueryReply = new JVMQueryReply();
        jvmQueryReply.setCountry("France");
        jvmQueryReply.setCurrencySymbol("€");
        jvmQueryReply.setFormattedDate("vendredi-10-octobre-2008-14h-28");
        jvmQueryReply.setLanguage("français");
        jvmQueryReply.getEnvVarValues().add(
                System.getenv("JAVA_HOME"));
        jvmQueryReply.getEnvVarValues().add(
                System.getenv("LEGSTAR_HOME"));
        return jvmQueryReply;
    }

    /**
     * Check that data object contains the expected values.
     * This assumes the Locale is France
     * @param jvmQueryReply the java object to check
     */
    public static void checkJavaObjectReplyFrance(final JVMQueryReply jvmQueryReply) {
        assertEquals("France", jvmQueryReply.getCountry());
        assertEquals("€", jvmQueryReply.getCurrencySymbol());
        assertEquals("français", jvmQueryReply.getLanguage());
        assertTrue(System.getenv("JAVA_HOME").startsWith(jvmQueryReply.getEnvVarValues().get(0)));
        assertTrue(System.getenv("LEGSTAR_HOME").startsWith(jvmQueryReply.getEnvVarValues().get(1)));
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexReplyFrance() { 

        return JVMQUERYREPLY_HOST_BYTES;
    }
    /**
     * @return an instance of a valued java object.
     */
    public static JVMQueryRequest getJavaObjectRequest() {
        JVMQueryRequest jvmQueryRequest = new JVMQueryRequest();
        jvmQueryRequest.getEnvVarNames().add("JAVA_HOME");
        jvmQueryRequest.getEnvVarNames().add("LEGSTAR_HOME");
        return jvmQueryRequest;
    }
    
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexRequest() {
        return
        "00000002"
        + "d1c1e5c16dc8d6d4c54040404040404040404040404040404040404040404040"
        + "d3c5c7e2e3c1d96dc8d6d4c54040404040404040404040404040404040404040";
    }
}

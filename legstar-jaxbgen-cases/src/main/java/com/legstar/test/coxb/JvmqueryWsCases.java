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

import junit.framework.TestCase;

import com.legstar.test.coxb.ws.jvmquery.ObjectFactory;
import com.legstar.test.coxb.ws.jvmquery.JvmQueryReply;
import com.legstar.test.coxb.ws.jvmquery.QueryJvmResponse;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public class JvmqueryWsCases extends TestCase {

    /** Expected raw mainframe response sample. */
    public static final String QUERYJVMRESPONSE_HOST_BYTES =
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
    private JvmqueryWsCases() {

    }

    /**
     * @return an instance of a valued java object.
     */
    public static QueryJvmResponse getJavaObject() {
        // Create and populate an instance of an object (JAXB annotated)
        JvmQueryReply jvmQueryReply = new JvmQueryReply();
        jvmQueryReply.setCountry("France");
        jvmQueryReply.setCurrencySymbol("€");
        jvmQueryReply.setFormattedDate("vendredi 10 octobre 2008 14 h 28");
        jvmQueryReply.setLanguage("français");
        jvmQueryReply.getEnvVarValues().add(
                "D:\\Legsem\\Legstar\\jboss\\mlittle\\product\\build\\jbossesb-server-4.4.GA");
        jvmQueryReply.getEnvVarValues().add("C:\\Program Files\\Java\\jdk1.6.0");

        QueryJvmResponse queryJvmResponse = new QueryJvmResponse();
        queryJvmResponse.setReturn(jvmQueryReply);
        return queryJvmResponse;
    }

    /**
     * Check that data object contains the expected values.
     * @param queryJvmResponse the java object to check
     */
    public static void checkJavaObject(final QueryJvmResponse queryJvmResponse) {
        assertEquals("France", queryJvmResponse.getReturn().getCountry());
        assertEquals("€", queryJvmResponse.getReturn().getCurrencySymbol());
        assertEquals("français", queryJvmResponse.getReturn().getLanguage());
        assertEquals("D:\\Legsem\\Legstar\\jboss\\mlittle\\", queryJvmResponse.getReturn().getEnvVarValues().get(0));
        assertEquals("C:\\Program Files\\Java\\jdk1.6.0", queryJvmResponse.getReturn().getEnvVarValues().get(1));
    }
    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHex() { 

        return QUERYJVMRESPONSE_HOST_BYTES;
    }

    /**
     * @return a JAXB object factory for this type of object
     */
    public static Object getFactory() {
        return new ObjectFactory();
    }

}

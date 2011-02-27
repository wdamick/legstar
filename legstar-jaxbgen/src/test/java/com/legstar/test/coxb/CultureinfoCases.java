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

import java.math.BigDecimal;

import com.legstar.coxb.host.HostData;
import com.legstar.test.coxb.cultureinfo.CultureInfoParameters;
import com.legstar.test.coxb.cultureinfo.CultureInfoReply;
import com.legstar.test.coxb.cultureinfo.GetInfo;
import com.legstar.test.coxb.cultureinfo.ObjectFactory;
import com.legstar.test.coxb.cultureinfo.ServerCultureInfo;

import junit.framework.TestCase;

/**
 * Provides data samples for testing throughout LegStar.  
 */
public class CultureinfoCases extends TestCase {


    /** Utility class. */
    private CultureinfoCases() {

    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexRequestFr() { 

        return
        /* F R - f r                                                     275.36     */
        "c6d9608699404040404040404040404040404040404040404040404040404040000027536f";
    }

    /**
     * @return a hexadecimal representation of host data.
     */
    public static String getHostBytesHexReplyFr() { 

        /** Sample raw mainframe response sample. */
        return
        /* €                                                     275.36   */
        "9f40404040404040404040404040404040404040404040404040404040404040"
        /*  F r a n c e                                                    */
        + "c699819583854040404040404040404040404040404040404040404040404040"
        /*  f r a n ç a i s                                                */
        + "86998195488189a2404040404040404040404040404040404040404040404040"
        /*  m a r d i   2 8   o c t o b r e   2 0 0 8   1 3   h   4 5   C E*/
        + "948199848940f2f8409683a39682998540f2f0f0f840f1f3408840f4f540c3c5"
        /*  2 7 5 , 3 6                                                    */
        + "f2f7f56bf3f64040404040404040404040404040404040404040404040404040"
        /*  f r - F R                                                      */
        + "869960c6d9404040404040404040404040404040404040404040404040404040"
        /*  F r a n c e                                                    */
        + "c699819583854040404040404040404040404040404040404040404040404040"
        /*  f r a n ç a i s                                                */
        + "86998195488189a2404040404040404040404040404040404040404040404040";
    }

    /**
     * Because there is a variable part in the response (date/time), we only
     * check the fixed portion of the reply.
     * @param hostBytes the host bytes to check
     */
    public static void checkHostBytesReplyFr(final byte[] hostBytes) {
        String response = HostData.toHexString(hostBytes);
        assertEquals(response.substring(0, 191),
                getHostBytesHexReplyFr().substring(0, 191));
        assertEquals(response.substring(256, 511),
                getHostBytesHexReplyFr().substring(256, 511));
    }

    /**
     * @return a java object
     */
    public static GetInfo getJavaObjectRequestFr() {
        ObjectFactory of = new ObjectFactory();
        GetInfo getInfo = of.createGetInfo();
        CultureInfoParameters cultureInfoParameters = of.createCultureInfoParameters();
        cultureInfoParameters.setCultureCode("fr-FR");
        cultureInfoParameters.setDecimalNumber(new BigDecimal("125645.62"));
        getInfo.setArg0(cultureInfoParameters);
        return getInfo;

    }

    /**
     * @return a java object
     */
    public static CultureInfoReply getJavaObjectResponseFr() {
        CultureInfoReply response = new CultureInfoReply();
        response.setCurrencySymbol("€");
        response.setDisplayCountry("France");
        response.setDisplayLanguage("French");
        response.setFormattedDate("18 avril 1992 18:38");
        response.setFormattedDecimalNumber("125.645,62");
        ServerCultureInfo sci = new ServerCultureInfo();
        sci.setCultureCode("en-US");
        sci.setDisplayCountry("United States");
        sci.setDisplayLanguage("English");
        response.setServerCultureInfo(sci);
        return response;
    }

    /**
     * @return a JAXB object factory for this type of object
     */
    public static Object getFactory() {
        return new ObjectFactory();
    }

}

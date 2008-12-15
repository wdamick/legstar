/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.test.coxb;

import com.legstar.coxb.CobolContext;
import com.legstar.coxb.ICobolComplexBinding;
import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.visitor.CobolMarshalVisitor;
import com.legstar.test.coxb.ws.jvmquery.JvmQueryReply;
import com.legstar.test.coxb.ws.jvmquery.QueryJvmResponse;
import com.legstar.test.coxb.ws.jvmquery.bind.QueryJvmResponseBinding;

import junit.framework.TestCase;

public class MarshalJvmqueryWsTest extends TestCase {

    /** Expected raw mainframe response sample. */
    public static final String EXPECTED_MAINFRAME_RESPONSE_DATA =
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

    public void testJvmqueryWs() throws Exception {

        // Create and populate an instance of an object (JAXB annotated)
        JvmQueryReply jvmQueryReply = new JvmQueryReply();
        jvmQueryReply.setCountry("France");
        jvmQueryReply.setCurrencySymbol("€");
        jvmQueryReply.setFormattedDate("vendredi-10-octobre-2008-14h-28");
        jvmQueryReply.setLanguage("français");
        jvmQueryReply.getEnvVarValues().add("D:\\Legsem\\Legstar\\jboss\\mlittle\\product\\build\\jbossesb-server-4.4.GA");
        jvmQueryReply.getEnvVarValues().add("C:\\Program Files\\Java\\jdk1.6.0_10");

        QueryJvmResponse queryJvmResponse = new QueryJvmResponse();
        queryJvmResponse.setReturn(jvmQueryReply);

        ICobolComplexBinding binding = new QueryJvmResponseBinding(queryJvmResponse);

        /* Convert Java data object to a host byte array */
        byte[] hostBytes = new byte[binding.calcByteLength()];
        CobolContext cobolContext = new CobolContext();
        CobolSimpleConverters cc = new CobolSimpleConverters(cobolContext);
        CobolMarshalVisitor mv =
            new CobolMarshalVisitor(hostBytes, 0, cc);
        binding.accept(mv);

        /* check */
        assertEquals(196,  mv.getOffset());
        assertEquals(EXPECTED_MAINFRAME_RESPONSE_DATA.substring(0, 131),
                HostData.toHexString(hostBytes).substring(0, 131));

    }
}

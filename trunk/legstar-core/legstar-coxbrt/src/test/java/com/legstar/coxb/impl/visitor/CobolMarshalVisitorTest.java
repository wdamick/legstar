/*******************************************************************************
 * Copyright (c) 2011 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.impl.visitor;

import java.math.BigDecimal;

import junit.framework.TestCase;

import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.test.coxb.coxb137.BoolPojo;
import com.legstar.test.coxb.pojo156.jaxb.SomeClass;
import com.legstar.test.coxb.pojo156.jaxb.SomeItem;

/**
 * There are several marshaling tests elsewhere in this project. Here we focus
 * on special cases.
 * 
 */
public class CobolMarshalVisitorTest extends TestCase {

    /**
     * Tests related to http://code.google.com/p/legstar/issues/detail?id=137.
     * 
     * @throws Exception if unmarshaling fails
     */
    public void testBooleanField() throws Exception {
        BoolPojo boolPojo = new BoolPojo();
        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new com.legstar.test.coxb.coxb137.ObjectFactory(), boolPojo);

        byte[] hostBytes = new byte[6];
        boolPojo.setABoolean(true);
        CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0,
                new CobolSimpleConverters());
        ccem.accept(mv);
        assertEquals("000000000001", HostData.toHexString(hostBytes));

        hostBytes = new byte[8];
        boolPojo.setABoolean(true);
        boolPojo.getBooleanList().add(false);
        mv = new CobolMarshalVisitor(hostBytes, 0, new CobolSimpleConverters());
        ccem.accept(mv);
        assertEquals("0000000100000001", HostData.toHexString(hostBytes));

    }

    /**
     * Tests that an implicit dynamic counter is marshaled correctly.
     * 
     * @throws Exception if test fails
     */
    public void testDynCountersPojo() throws Exception {
        SomeClass someClass = new SomeClass();
        for (int i = 0; i < 2; i++) {
            SomeItem item = new SomeItem();
            item.setAmount(new BigDecimal(125.23 * i));
            item.setLabel("item" + i);
            item.setNumber(i);
            someClass.getItems().add(item);
        }

        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new com.legstar.test.coxb.pojo156.jaxb.ObjectFactory(),
                someClass);
        byte[] hostBytes = new byte[86];
        CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0,
                new CobolSimpleConverters());
        ccem.accept(mv);
        assertEquals(86, mv.getOffset());
        assertEquals(
                "00000002"
                        + "000000000c89a38594f040404040404040404040404040404040404040404040404040404000000000"
                        + "000012523c89a38594f140404040404040404040404040404040404040404040404040404000000001",
                HostData.toHexString(hostBytes));
    }

    /**
     * Tests that an explicit dynamic counter is marshaled correctly.
     * 
     * @throws Exception if test fails
     */
    public void testDynCountersCobol() throws Exception {
        com.legstar.test.coxb.cob156.A parent = new com.legstar.test.coxb.cob156.A();
        // Don't set the explicit B counter, it should be automatically valued
        com.legstar.test.coxb.cob156.C child = new com.legstar.test.coxb.cob156.C();
        for (int i = 0; i < 3; i++) {
            child.getD().add(Integer.toString(i));
        }
        parent.setC(child);

        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new com.legstar.test.coxb.cob156.ObjectFactory(), parent);
        byte[] hostBytes = new byte[6];
        CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0,
                new CobolSimpleConverters());
        ccem.accept(mv);
        assertEquals(6, mv.getOffset());
        assertEquals("f0f0f3f0f1f2", HostData.toHexString(hostBytes));

    }

    /**
     * Tests that alternatives with different sizes are handled correctly upon
     * Marshaling.
     * 
     * <pre>
     *         01  DFHCOMMAREA.
     *      10 CONFPRTY.
     *         15 CONFPRTY-FRMTQ.
     *            20 CONFPRTY-FRMTQ-DATA                    PIC X(140).
     *         15 CONFPRTY-FRMTR REDEFINES CONFPRTY-FRMTQ.
     *            20  CONFPRTY-FRMTR-DATA                   PIC X(42).
     *         15 CONFPRTY-FRMTP REDEFINES CONFPRTY-FRMTQ.
     *            20  CONFPRTY-FRMTP-DATA                   PIC X(11).
     *      10 ALTEPRTY.
     *         15 ALTE-PRTY-QUAL                            PIC X(04).
     * </pre>
     * 
     * @throws Exception if test fails
     */
    public void testRedefinesDiffSizeAlternatives() throws Exception {
        com.legstar.test.coxb.issue162.Dfhcommarea dfhcommarea = new com.legstar.test.coxb.issue162.Dfhcommarea();
        com.legstar.test.coxb.issue162.Confprty confPrty = new com.legstar.test.coxb.issue162.Confprty();
        com.legstar.test.coxb.issue162.ConfprtyFrmtp confprtyFrmtp = new com.legstar.test.coxb.issue162.ConfprtyFrmtp();
        confprtyFrmtp.setConfprtyFrmtpData("12345678901");
        confPrty.setConfprtyFrmtp(confprtyFrmtp);
        com.legstar.test.coxb.issue162.Alteprty altePrty = new com.legstar.test.coxb.issue162.Alteprty();
        altePrty.setAltePrtyQual("MMMM");
        dfhcommarea.setConfprty(confPrty);
        dfhcommarea.setAlteprty(altePrty);

        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new com.legstar.test.coxb.issue162.ObjectFactory(), dfhcommarea);
        byte[] hostBytes = new byte[144];
        CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0,
                new CobolSimpleConverters());
        ccem.accept(mv);
        assertEquals(144, mv.getOffset());
        assertEquals(
                "f1f2f3f4f5f6f7f8f9f0f1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
                        + "d4d4d4d4", HostData.toHexString(hostBytes));

    }

    /**
     * Multiple nested alternatives with different sizes.
     * <p/>
     * 
     * @see https://code.google.com/p/legstar/issues/detail?id=185
     * 
     *      <pre>
     *  01  COMMAREA.
     *      10 OUTER-REDEFINES-LONG PIC X(10).
     *      10 OUTER-REDEFINES-SHORT 
     *            REDEFINES OUTER-REDEFINES-LONG.
     *         15 INNER-REDEFINES-LONG PIC X(5).
     *         15 INNER-REDEFINES-SHORT
     *            REDEFINES INNER-REDEFINES-LONG PIC X(3).
     *      10 FOOTER PIC X.
     * </pre>
     * 
     * @throws Exception if test fails
     */
    public void testIssue185() throws Exception {

        com.legstar.test.coxb.issue185.Commarea commarea = new com.legstar.test.coxb.issue185.Commarea();
        com.legstar.test.coxb.issue185.OuterRedefinesShort ors = new com.legstar.test.coxb.issue185.OuterRedefinesShort();
        ors.setInnerRedefinesShort("ABC");
        commarea.setOuterRedefinesShort(ors);
        commarea.setFooter("Z");

        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new com.legstar.test.coxb.issue185.ObjectFactory(), commarea);
        byte[] hostBytes = new byte[11];
        CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0,
                new CobolSimpleConverters());
        ccem.accept(mv);
        assertEquals(11, mv.getOffset());
        assertEquals("c1c2c300000000000000e9", HostData.toHexString(hostBytes));

        commarea = new com.legstar.test.coxb.issue185.Commarea();
        commarea.setOuterRedefinesLong("ABC");
        commarea.setFooter("Z");

        ccem = new CComplexReflectBinding(
                new com.legstar.test.coxb.issue185.ObjectFactory(), commarea);

        mv = new CobolMarshalVisitor(hostBytes, 0, new CobolSimpleConverters());
        ccem.accept(mv);
        assertEquals(11, mv.getOffset());
        assertEquals("c1c2c340404040404040e9", HostData.toHexString(hostBytes));

    }

    /**
     * Here the ODO counter is within a REDEFINE.
     */
    public void testIssue186() throws Exception {
        com.legstar.test.coxb.issue186.Ardo02Record ardo02Record = new com.legstar.test.coxb.issue186.Ardo02Record();
        com.legstar.test.coxb.issue186.AlternativeA alternativeA = new com.legstar.test.coxb.issue186.AlternativeA();
        alternativeA.setOdoCounter(1);
        ardo02Record.setAlternativeA(alternativeA);
        com.legstar.test.coxb.issue186.OdoArray item_1 = new com.legstar.test.coxb.issue186.OdoArray();
        item_1.setFiller10("A");
        ardo02Record.getOdoArray().add(item_1);

        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new com.legstar.test.coxb.issue186.ObjectFactory(),
                ardo02Record);
        byte[] hostBytes = new byte[3];
        CobolMarshalVisitor mv = new CobolMarshalVisitor(hostBytes, 0,
                new CobolSimpleConverters());
        ccem.accept(mv);
        assertEquals(3, mv.getOffset());
        assertEquals("0001c1", HostData.toHexString(hostBytes));
    }
}

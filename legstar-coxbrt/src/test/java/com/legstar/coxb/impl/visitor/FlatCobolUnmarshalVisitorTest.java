package com.legstar.coxb.impl.visitor;

import java.math.BigDecimal;
import java.util.Map;

import com.legstar.coxb.convert.simple.CobolSimpleConverters;
import com.legstar.coxb.host.HostData;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;

/**
 * Test FlatCobolUnmarshalVisitor.
 * 
 */
public class FlatCobolUnmarshalVisitorTest extends AbstractVisitorTest {

    /**
     * Test with a simple flat structure.
     * 
     * @throws Exception if test fails
     */
    public void testFlatStructure() throws Exception {
        byte[] hostBytes = HostData
                .toByteArray("F0F0F0F0F4F3D5C1D4C5F0F0F0F0F4F3404040404040404040400215000FF0");
        FlatCobolUnmarshalVisitor uv = new FlatCobolUnmarshalVisitor(hostBytes,
                0, new CobolSimpleConverters());

        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new Flat01RecordFactory(), Flat01Record.class);
        ccem.accept(uv);
        assertEquals("{ComNumber=43, ComName=NAME000043, ComAmount=2150.00}",
                uv.getKeyValues().toString());

    }

    /**
     * Test with a simple array.
     * 
     * @throws Exception if test fails
     */
    public void testSimpleArray() throws Exception {
        byte[] hostBytes = HostData
                .toByteArray("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F003E001F0014000F000C");
        FlatCobolUnmarshalVisitor uv = new FlatCobolUnmarshalVisitor(hostBytes,
                0, new CobolSimpleConverters());

        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new Flat02RecordFactory(), Flat02Record.class);
        ccem.accept(uv);
        assertEquals(
                "{ComNumber=62, ComName=NAME000062, ComAmount=3100.00,"
                        + " ComArray_0=62, ComArray_1=31, ComArray_2=20, ComArray_3=15, ComArray_4=12}",
                uv.getKeyValues().toString());

    }

    /**
     * Test structure with a complex array.
     * 
     * @throws Exception if test fails
     */
    public void testComplexArray() throws Exception {
        byte[] hostBytes = HostData
                .toByteArray("F0F0F0F0F6F2D5C1D4C5F0F0F0F0F6F2404040404040404040400310000F003EC1C2001FC1C20014C1C2000FC1C2000CC1C2");
        FlatCobolUnmarshalVisitor uv = new FlatCobolUnmarshalVisitor(hostBytes,
                0, new CobolSimpleConverters());

        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new Stru03RecordFactory(), Stru03Record.class);
        ccem.accept(uv);
        assertEquals("{ComNumber=62, ComName=NAME000062, ComAmount=3100.00,"
                + " ComItem1_0=62, ComItem2_0=AB,"
                + " ComItem1_1=31, ComItem2_1=AB,"
                + " ComItem1_2=20, ComItem2_2=AB,"
                + " ComItem1_3=15, ComItem2_3=AB,"
                + " ComItem1_4=12, ComItem2_4=AB}", uv.getKeyValues()
                .toString());

    }

    /**
     * Test deep hierarchy with complex arrays.
     * 
     * @throws Exception if test fails
     */
    public void testDeepComplexArray() throws Exception {
        byte[] hostBytes = HostData
                .toByteArray("0190000F00090006C2C5C5C2C4C40001900FC2C2C5C4C5C30000950F0003000000020013000CC2C4C2C1C5C40003800FC1C5C2C2C4C10001900F000600000005001C0013C1C5C2C5C1C30005700FC4C2C3C3C3C20002850F0009000000080023750F");
        FlatCobolUnmarshalVisitor uv = new FlatCobolUnmarshalVisitor(hostBytes,
                0, new CobolSimpleConverters());

        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new Stru04RecordFactory(), Stru04Record.class);
        ccem.accept(uv);
        Map < String, Object > values = uv.getKeyValues();
        assertEquals(new BigDecimal("1900.00"), values.get("ComItem1"));
        assertEquals(new Short("9"), values.get("ComItem2_0"));
        assertEquals(new Short("6"), values.get("ComItem3_0"));
        assertEquals("B", values.get("ComItem4_0_0"));
        assertEquals("E", values.get("ComArray3_0_0_0"));
        assertEquals("E", values.get("ComArray3_0_0_1"));
        assertEquals("B", values.get("ComArray3_0_0_2"));
        assertEquals("D", values.get("ComArray3_0_0_3"));
        assertEquals("D", values.get("ComArray3_0_0_4"));
        assertEquals(new BigDecimal("19.00"), values.get("ComItem5_0_0"));
        assertEquals("B", values.get("ComItem4_0_1"));
        assertEquals("B", values.get("ComArray3_0_1_0"));
        assertEquals("E", values.get("ComArray3_0_1_1"));
        assertEquals("D", values.get("ComArray3_0_1_2"));
        assertEquals("E", values.get("ComArray3_0_1_3"));
        assertEquals("C", values.get("ComArray3_0_1_4"));
        assertEquals(new BigDecimal("9.50"), values.get("ComItem5_0_1"));
        assertEquals(new Short("3"), values.get("ComItem6_0"));
        assertEquals(new Integer("2"), values.get("ComItem7_0"));
        assertEquals(new Short("19"), values.get("ComItem2_1"));
        assertEquals(new Short("12"), values.get("ComItem3_1"));
        assertEquals("B", values.get("ComItem4_1_0"));
        assertEquals("D", values.get("ComArray3_1_0_0"));
        assertEquals("B", values.get("ComArray3_1_0_1"));
        assertEquals("A", values.get("ComArray3_1_0_2"));
        assertEquals("E", values.get("ComArray3_1_0_3"));
        assertEquals("D", values.get("ComArray3_1_0_4"));
        assertEquals(new BigDecimal("38.00"), values.get("ComItem5_1_0"));
        assertEquals("A", values.get("ComItem4_1_1"));
        assertEquals("E", values.get("ComArray3_1_1_0"));
        assertEquals("B", values.get("ComArray3_1_1_1"));
        assertEquals("B", values.get("ComArray3_1_1_2"));
        assertEquals("D", values.get("ComArray3_1_1_3"));
        assertEquals("A", values.get("ComArray3_1_1_4"));
        assertEquals(new BigDecimal("19.00"), values.get("ComItem5_1_1"));
        assertEquals(new Short("6"), values.get("ComItem6_1"));
        assertEquals(new Integer("5"), values.get("ComItem7_1"));
        assertEquals(new Short("28"), values.get("ComItem2_2"));
        assertEquals(new Short("19"), values.get("ComItem3_2"));
        assertEquals("A", values.get("ComItem4_2_0"));
        assertEquals("E", values.get("ComArray3_2_0_0"));
        assertEquals("B", values.get("ComArray3_2_0_1"));
        assertEquals("E", values.get("ComArray3_2_0_2"));
        assertEquals("A", values.get("ComArray3_2_0_3"));
        assertEquals("C", values.get("ComArray3_2_0_4"));
        assertEquals(new BigDecimal("57.00"), values.get("ComItem5_2_0"));
        assertEquals("D", values.get("ComItem4_2_1"));
        assertEquals("B", values.get("ComArray3_2_1_0"));
        assertEquals("C", values.get("ComArray3_2_1_1"));
        assertEquals("C", values.get("ComArray3_2_1_2"));
        assertEquals("C", values.get("ComArray3_2_1_3"));
        assertEquals("B", values.get("ComArray3_2_1_4"));
        assertEquals(new BigDecimal("28.50"), values.get("ComItem5_2_1"));
        assertEquals(new Short("9"), values.get("ComItem6_2"));
        assertEquals(new Integer("8"), values.get("ComItem7_2"));
        assertEquals(new BigDecimal("237.50"), values.get("ComItem8"));

    }

    /**
     * Test a redefine with 2 alternatives.
     * 
     * @throws Exception if test fails
     */
    public void testRedefines() throws Exception {

        /* First alternative */
        byte[] hostBytes = HostData
                .toByteArray("00010250000F40404040404000010260000F404040404040");
        FlatCobolUnmarshalVisitor uv = new FlatCobolUnmarshalVisitor(hostBytes,
                0, new CobolSimpleConverters());

        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new Rdef01RecordFactory(), Rdef01Record.class);
        ccem.accept(uv);
        Map < String, Object > values = uv.getKeyValues();
        assertEquals("{ComSelect=1, ComAmount=2500.00}", values.toString());

        /* Second alternative */
        hostBytes = HostData
                .toByteArray("0000D5C1D4C5F0F0F0F0F0F50000D5C1D4C5F0F0F0F0F2F1");
        uv = new FlatCobolUnmarshalVisitor(hostBytes, 0,
                new CobolSimpleConverters());
        ccem.accept(uv);
        values = uv.getKeyValues();
        assertEquals("{ComSelect=0, ComName=NAME000005}", values.toString());

        /* Another record with first alternative */
        hostBytes = HostData
                .toByteArray("00010250000F40404040404000010260000F404040404040");
        uv = new FlatCobolUnmarshalVisitor(hostBytes, 0,
                new CobolSimpleConverters());
        ccem.accept(uv);
        values = uv.getKeyValues();
        assertEquals("{ComSelect=1, ComAmount=2500.00}", values.toString());

    }

    /**
     * Test with ambiguous names.
     * 
     * @throws Exception if test fails
     */
    public void testNameConflicts() throws Exception {

        byte[] hostBytes = HostData.toByteArray("0090000F00040009000DC1C2C3C4");
        FlatCobolUnmarshalVisitor uv = new FlatCobolUnmarshalVisitor(hostBytes,
                0, new CobolSimpleConverters());

        CComplexReflectBinding ccem = new CComplexReflectBinding(
                new Stru05RecordFactory(), Stru05Record.class);
        ccem.accept(uv);
        Map < String, Object > values = uv.getKeyValues();
        assertEquals("{ComItemB=900.00,"
                + " ComItemB_0=4, ComItemB_1=9, ComItemB_2=13"
                + ", ComItemE_ComItemB=ABCD}", values.toString());
    }
}

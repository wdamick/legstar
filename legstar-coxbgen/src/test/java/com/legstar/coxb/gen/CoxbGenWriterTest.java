/*******************************************************************************
 * Copyright (c) 2008 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.coxb.gen;

import com.legstar.codegen.CodeGenUtil;
import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.util.JaxbUtil;

/**
 * Test the code generation writer.
 *
 */
public class CoxbGenWriterTest extends AbstractTestTemplate {

    /** @{inheritDoc}*/
    public void setUp() {
        super.setUp();
        CodeGenUtil.checkDirectory(GEN_SRC_DIR, true);
    }

    /**
     * A complex type case.
     * @throws Exception if generation fails
     */
    public void testGenAlltypes() throws Exception {
        com.legstar.test.coxb.alltypes.ObjectFactory objectFactory
        = new com.legstar.test.coxb.alltypes.ObjectFactory();

        CComplexReflectBinding ce = new CComplexReflectBinding(
                objectFactory,
                JaxbUtil.loadClass("com.legstar.test.coxb.alltypes.Dfhcommarea"));

        CoxbGenModel coxbGenContext = new CoxbGenModel();
        coxbGenContext.setCoxbSrcDir(GEN_SRC_DIR);
        coxbGenContext.setJaxbPackageName("com.legstar.test.coxb.alltypes");
        coxbGenContext.setCoxbPackageName("com.legstar.test.coxb.alltypes.bind");

        CoxbGenWriter writer = new CoxbGenWriter(coxbGenContext);

        writer.write(ce);
        String resStr = getSource(GEN_SRC_DIR, "/com/legstar/test/coxb/alltypes/bind/DfhcommareaBinding.java");

        assertTrue(resStr.contains("import com.legstar.coxb.ICobolStringBinding;"));
    }

    /**
     * A complex type containing a redefine case.
     * @throws Exception if generation fails
     */
    public void testGenRedsimpt() throws Exception {
        com.legstar.test.coxb.redsimpt.ObjectFactory objectFactory
        = new com.legstar.test.coxb.redsimpt.ObjectFactory();

        CComplexReflectBinding ce = new CComplexReflectBinding(
                objectFactory,
                JaxbUtil.loadClass("com.legstar.test.coxb.redsimpt.Dfhcommarea"));

        ICobolChoiceBinding cc = (ICobolChoiceBinding) ce.getChildrenList().get(0);

        CoxbGenModel coxbGenContext = new CoxbGenModel();
        coxbGenContext.setCoxbSrcDir(GEN_SRC_DIR);
        coxbGenContext.setJaxbPackageName("com.legstar.test.coxb.redsimpt");
        coxbGenContext.setCoxbPackageName("com.legstar.test.coxb.redsimpt.bind");

        CoxbGenWriter writer = new CoxbGenWriter(coxbGenContext);

        writer.write(cc);
        String resStr = getSource(GEN_SRC_DIR, "/com/legstar/test/coxb/redsimpt/bind/CDefinition1ChoiceBinding.java");

        assertTrue(resStr.contains("import com.legstar.coxb.ICobolComplexBinding;"));
    }

    /**
     * A complex array type case.
     * @throws Exception if generation fails
     */
    public void testGenArrayssm() throws Exception {
        com.legstar.test.coxb.arrayssm.ObjectFactory objectFactory
        = new com.legstar.test.coxb.arrayssm.ObjectFactory();

        CComplexReflectBinding ce = new CComplexReflectBinding(
                objectFactory,
                JaxbUtil.loadClass("com.legstar.test.coxb.arrayssm.Dfhcommarea"));

        ICobolArrayComplexBinding ca = (ICobolArrayComplexBinding) ce.getChildrenList().get(1);

        CoxbGenModel coxbGenContext = new CoxbGenModel();
        coxbGenContext.setCoxbSrcDir(GEN_SRC_DIR);
        coxbGenContext.setJaxbPackageName("com.legstar.test.coxb.arrayssm");
        coxbGenContext.setCoxbPackageName("com.legstar.test.coxb.arrayssm.bind");

        CoxbGenWriter writer = new CoxbGenWriter(coxbGenContext);

        writer.write(ca);
        String resStr = getSource(GEN_SRC_DIR, "/com/legstar/test/coxb/arrayssm/bind/TableComplexWrapperBinding.java");

        assertTrue(resStr.contains("import com.legstar.coxb.common.CArrayComplexBinding;"));
    }

    /**
     * A choice strategy type case.
     * @throws Exception if generation fails
     */
    public void testGenChoiceStrategy() throws Exception {
        com.legstar.test.coxb.redsimpt.ObjectFactory objectFactory
        = new com.legstar.test.coxb.redsimpt.ObjectFactory();

        CComplexReflectBinding ce = new CComplexReflectBinding(
                objectFactory,
                JaxbUtil.loadClass("com.legstar.test.coxb.redsimpt.Dfhcommarea"));

        ICobolChoiceBinding cc = (ICobolChoiceBinding) ce.getChildrenList().get(0);

        CoxbGenModel coxbGenContext = new CoxbGenModel();
        coxbGenContext.setCoxbSrcDir(GEN_SRC_DIR);
        coxbGenContext.setJaxbPackageName("com.legstar.test.coxb.redsimpt");
        coxbGenContext.setCoxbPackageName("com.legstar.test.coxb.redsimpt.bind");

        CoxbGenWriter writer = new CoxbGenWriter(coxbGenContext);

        /* Do it twice to check the backup mechanism */
        writer.writeChoiceStrategy(cc, "Unmarshal", "com.legstar.coxb.cust.redsimpt.ChoiceSelector");
        writer.writeChoiceStrategy(cc, "Unmarshal", "com.legstar.coxb.cust.redsimpt.ChoiceSelector");

        String resStr = getSource(GEN_SRC_DIR, "/com/legstar/coxb/cust/redsimpt/ChoiceSelector.java.new");
        assertTrue(resStr.contains("public class ChoiceSelector implements ICobolUnmarshalChoiceStrategy {"));
    }

    /**
     * Generate a host to java transformer case.
     * @throws Exception if generation fails
     */
    public void testGenHostToJavaTransformer() throws Exception {

        com.legstar.test.coxb.lsfileae.ObjectFactory objectFactory
        = new com.legstar.test.coxb.lsfileae.ObjectFactory();

        CComplexReflectBinding ce = new CComplexReflectBinding(
                objectFactory,
                JaxbUtil.loadClass("com.legstar.test.coxb.lsfileae.Dfhcommarea"));

        CoxbGenModel coxbGenContext = new CoxbGenModel();
        coxbGenContext.setCoxbSrcDir(GEN_SRC_DIR);
        coxbGenContext.setJaxbPackageName("com.legstar.test.coxb.lsfileae");
        coxbGenContext.setCoxbPackageName("com.legstar.test.coxb.lsfileae.bind");

        CoxbGenWriter writer = new CoxbGenWriter(coxbGenContext);
        writer.writeHostToJavaTransformer(ce);
        String resStr = getSource(GEN_SRC_DIR,
                "/com/legstar/test/coxb/lsfileae/bind/DfhcommareaHostToJavaTransformer.java");

        assertTrue(resStr.contains("DfhcommareaHostToJavaTransformer transformer ="
                + " new DfhcommareaHostToJavaTransformer();"));
        assertTrue(resStr.contains("Dfhcommarea javaValue = (Dfhcommarea) transformer.transform(hostByteArray);"));
        assertTrue(resStr.contains("public class DfhcommareaHostToJavaTransformer"
                + " extends AbstractHostToJavaTransformer {"));
        assertTrue(resStr.contains("public DfhcommareaHostToJavaTransformer() {"));
        assertTrue(resStr.contains("public DfhcommareaHostToJavaTransformer(final CobolContext cobolContext) {"));
        assertTrue(resStr.contains("public DfhcommareaHostToJavaTransformer(final String hostCharset) {"));
        assertTrue(resStr.contains("return new DfhcommareaBinding();"));
    }

    /**
     * Generate a java to host transformer case.
     * @throws Exception if generation fails
     */
    public void testGenJavaToHostTransformer() throws Exception {

        com.legstar.test.coxb.lsfileae.ObjectFactory objectFactory
        = new com.legstar.test.coxb.lsfileae.ObjectFactory();

        CComplexReflectBinding ce = new CComplexReflectBinding(
                objectFactory,
                JaxbUtil.loadClass("com.legstar.test.coxb.lsfileae.Dfhcommarea"));

        CoxbGenModel coxbGenContext = new CoxbGenModel();
        coxbGenContext.setCoxbSrcDir(GEN_SRC_DIR);
        coxbGenContext.setJaxbPackageName("com.legstar.test.coxb.lsfileae");
        coxbGenContext.setCoxbPackageName("com.legstar.test.coxb.lsfileae.bind");

        CoxbGenWriter writer = new CoxbGenWriter(coxbGenContext);
        writer.writeJavaToHostTransformer(ce);
        String resStr = getSource(GEN_SRC_DIR,
                "/com/legstar/test/coxb/lsfileae/bind/DfhcommareaJavaToHostTransformer.java");

        assertTrue(resStr.contains("DfhcommareaJavaToHostTransformer transformer"
                + " = new DfhcommareaJavaToHostTransformer();"));
        assertTrue(resStr.contains("byte[] hostByteArray = (Dfhcommarea) transformer.transform(javaValue);"));
        assertTrue(resStr.contains("public class DfhcommareaJavaToHostTransformer extends"
                + " AbstractJavaToHostTransformer {"));
        assertTrue(resStr.contains("public DfhcommareaJavaToHostTransformer() {"));
        assertTrue(resStr.contains("public DfhcommareaJavaToHostTransformer(final CobolContext cobolContext) {"));
        assertTrue(resStr.contains("public DfhcommareaJavaToHostTransformer(final String hostCharset) {"));
        assertTrue(resStr.contains("return new DfhcommareaBinding();"));
    }

    /**
     * Generate a transformer provider case.
     * @throws Exception if generation fails
     */
    public void testGenTransformers() throws Exception {

        com.legstar.test.coxb.lsfileae.ObjectFactory objectFactory
        = new com.legstar.test.coxb.lsfileae.ObjectFactory();

        CComplexReflectBinding ce = new CComplexReflectBinding(
                objectFactory,
                JaxbUtil.loadClass("com.legstar.test.coxb.lsfileae.Dfhcommarea"));

        CoxbGenModel coxbGenContext = new CoxbGenModel();
        coxbGenContext.setCoxbSrcDir(GEN_SRC_DIR);
        coxbGenContext.setJaxbPackageName("com.legstar.test.coxb.lsfileae");
        coxbGenContext.setCoxbPackageName("com.legstar.test.coxb.lsfileae.bind");

        CoxbGenWriter writer = new CoxbGenWriter(coxbGenContext);
        writer.writeTransformers(ce);
        String resStr = getSource(GEN_SRC_DIR,
                "/com/legstar/test/coxb/lsfileae/bind/DfhcommareaTransformers.java");

        assertTrue(resStr.contains("package com.legstar.test.coxb.lsfileae.bind;"));
        assertTrue(resStr.contains("* Transformer provider for Dfhcommarea java data object."));
        assertTrue(resStr.contains(
            "public class DfhcommareaTransformers extends AbstractTransformers {"));
        assertTrue(resStr.contains("public DfhcommareaTransformers() {"));
        assertTrue(resStr.contains("super(new DfhcommareaJavaToHostTransformer(),"));
        assertTrue(resStr.contains("new DfhcommareaHostToJavaTransformer());"));
    }
}

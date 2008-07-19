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
package com.legstar.coxb.gen.vm;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.codegen.CodeGenHelper;
import com.legstar.codegen.CodeGenUtil;
import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.gen.CoxbGenModel;
import com.legstar.coxb.gen.CoxbHelper;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.util.JaxbUtil;

import junit.framework.TestCase;

public class VelocityTemplatesTest extends TestCase {

	/** This generator name. */
	private static final String BINDING_GENERATOR_NAME =
		"LegStar Binding generator";

	/** Parameters expected by the vlc templates.*/
	private Map < String, Object > mParameters;

	/** Logger. */
	private static final Log LOG = LogFactory.getLog(VelocityTemplatesTest.class);

	/** Code will be generated here. */
	private static final String GEN_SRC_DIR = "src/test/gen/java";

	public void setUp() throws Exception {
		CodeGenUtil.initVelocity();
		CodeGenUtil.checkDirectory(GEN_SRC_DIR, true);
		CoxbHelper coxbHelper = new CoxbHelper();
		CodeGenHelper helper = new CodeGenHelper();
		mParameters = new HashMap < String, Object >();
		mParameters.put("helper", helper);
		mParameters.put("coxbHelper", coxbHelper);
	}

	public void testGenAllTypes() throws Exception {

		com.legstar.test.coxb.lsfilead.ObjectFactory objectFactory
		= new com.legstar.test.coxb.lsfilead.ObjectFactory();

		CComplexReflectBinding ce = new CComplexReflectBinding(
				objectFactory,
				JaxbUtil.loadClass("com.legstar.test.coxb.alltypes.DfhcommareaType"));

		CoxbGenModel coxbContext = new CoxbGenModel();
		coxbContext.setJaxbPackageName("com.legstar.test.coxb.alltypes");
		coxbContext.setCoxbPackageName("com.legstar.test.coxb.alltypes.bind");
		mParameters.put("coxbContext", coxbContext);
		mParameters.put("binding-class-name", "DfhcommareaTypeBinding");

		CodeGenUtil.processTemplate(
				BINDING_GENERATOR_NAME,
				"vlc/coxb-bind-complex.vm",
				"binding", ce,
				mParameters,
				CodeGenUtil.getFile(GEN_SRC_DIR, "test.txt"));

		BufferedReader in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/test.txt"));
		String resStr = "";
		String str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolStringBinding;"));
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolArrayStringBinding;"));
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolBinaryBinding;"));
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolArrayBinaryBinding;"));
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolPackedDecimalBinding;"));
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolArrayPackedDecimalBinding;"));
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolOctetStreamBinding;"));
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolFloatBinding;"));
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolArrayFloatBinding;"));
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolDoubleBinding;"));
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolArrayDoubleBinding;"));
		assertTrue(resStr.contains("public DfhcommareaTypeBinding("));
		assertTrue(resStr.contains("final DfhcommareaType valueObject) {"));
		assertTrue(resStr.contains("this(\"\", \"\", null, valueObject);"));
		assertTrue(resStr.contains("import java.math.BigInteger;"));
		assertTrue(resStr.contains("import java.math.BigDecimal;"));
		assertTrue(resStr.contains("sString.setByteLength(4);"));
		assertTrue(resStr.contains("sString.setCobolName(\"S-STRING\");"));
		assertTrue(resStr.contains("sShort.setByteLength(2);"));
		assertTrue(resStr.contains("sShort.setCobolName(\"S-SHORT\");"));
		assertTrue(resStr.contains("sShort.setIsSigned(true);"));
		assertTrue(resStr.contains("aBinary.setByteLength(4);"));
		assertTrue(resStr.contains("aBinary.setCobolName(\"A-BINARY\");"));
		assertTrue(resStr.contains("aBinary.setMinOccurs(2);"));
		assertTrue(resStr.contains("sString.setObjectValue(mValueObject.getSString());"));
		assertTrue(resStr.contains("sUshort.setObjectValue(mValueObject.getSUshort());"));
		assertTrue(resStr.contains("aDec.setObjectValue(mValueObject.getADec());"));
		assertTrue(resStr.contains("aFloat.setObjectValue(mValueObject.getAFloat());"));
		assertTrue(resStr.contains("aDouble.setObjectValue(mValueObject.getADouble());"));

		assertTrue(resStr.contains("mValueObject.setSString((String) bindingValue);"));
		assertTrue(resStr.contains("mValueObject.getADouble().clear();"));
		assertTrue(resStr.contains("mValueObject.getADouble().addAll("));
		assertTrue(resStr.contains("(List < Double >) bindingValue);"));
	}

	public void testGenArrayssm() throws Exception {

		com.legstar.test.coxb.arrayssm.ObjectFactory objectFactory
		= new com.legstar.test.coxb.arrayssm.ObjectFactory();

		CComplexReflectBinding ce = new CComplexReflectBinding(
				objectFactory,
				JaxbUtil.loadClass("com.legstar.test.coxb.arrayssm.DfhcommareaType"));

		CoxbGenModel coxbContext = new CoxbGenModel();
		coxbContext.setJaxbPackageName("com.legstar.test.coxb.arrayssm");
		coxbContext.setCoxbPackageName("com.legstar.test.coxb.arrayssm.bind");
		mParameters.put("coxbContext", coxbContext);
		mParameters.put("binding-class-name", "DfhcommareaTypeBinding");

		CodeGenUtil.processTemplate(
				BINDING_GENERATOR_NAME,
				"vlc/coxb-bind-complex.vm",
				"binding", ce,
				mParameters,
				CodeGenUtil.getFile(GEN_SRC_DIR, "test.txt"));

		BufferedReader in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/test.txt"));
		String resStr = "";
		String str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("import java.util.List;"));
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolArrayComplexBinding;"));
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolArrayStringBinding;"));
		assertTrue(resStr.contains("import com.legstar.test.coxb.arrayssm.TableComplexType;"));

		assertTrue(resStr.contains("public ICobolArrayStringBinding tableSimple;"));
		assertTrue(resStr.contains("public ICobolArrayComplexBinding tableComplexWrapper;"));
		assertTrue(resStr.contains("public ICobolComplexBinding tableComplexWrapperItem;"));
		assertTrue(resStr.contains("public ICobolComplexBinding tableComplex2;"));
		assertTrue(resStr.contains("public ICobolArrayZonedDecimalBinding tableSimpleNumeric;"));

		assertTrue(resStr.contains("tableSimple = BF.createArrayStringBinding(\"TableSimple\","));
		assertTrue(resStr.contains("tableSimple.setByteLength(3);"));
		assertTrue(resStr.contains("tableSimple.setCobolName(\"TABLE-SIMPLE\");"));
		assertTrue(resStr.contains("tableSimple.setMinOccurs(2);"));
		assertTrue(resStr.contains("tableSimple.setMaxOccurs(2);"));
		assertTrue(resStr.contains("tableComplexWrapperItem = new TableComplexTypeBinding(\"TableComplexWrapperItem\","));
		assertTrue(resStr.contains("\"TableComplex\", this, null);"));
		assertTrue(resStr.contains("tableComplexWrapper = new TableComplexTypeWrapperBinding(\"TableComplexWrapper\","));
		assertTrue(resStr.contains("\"TableComplex\", this, tableComplexWrapperItem);"));

		assertTrue(resStr.contains("mValueObject.getTableComplex().clear();"));
		assertTrue(resStr.contains("mValueObject.getTableComplex().addAll("));
		assertTrue(resStr.contains("(List < TableComplexType >) bindingValue);"));
	}

	public void testGenArrayssmWrapper() throws Exception {

		com.legstar.test.coxb.arrayssm.ObjectFactory objectFactory
		= new com.legstar.test.coxb.arrayssm.ObjectFactory();

		CComplexReflectBinding ce = new CComplexReflectBinding(
				objectFactory,
				JaxbUtil.loadClass("com.legstar.test.coxb.arrayssm.DfhcommareaType"));

		ICobolArrayComplexBinding ca = (ICobolArrayComplexBinding) ce.getChildrenList().get(1);

		CoxbGenModel coxbContext = new CoxbGenModel();
		coxbContext.setJaxbPackageName("com.legstar.test.coxb.arrayssm");
		coxbContext.setCoxbPackageName("com.legstar.test.coxb.arrayssm.bind");
		mParameters.put("coxbContext", coxbContext);
		mParameters.put("binding-class-name", "TableComplexTypeWrapperBinding");

		CodeGenUtil.processTemplate(
				BINDING_GENERATOR_NAME,
				"vlc/coxb-bind-complex-array.vm",
				"binding", ca,
				mParameters,
				CodeGenUtil.getFile(GEN_SRC_DIR, "test.txt"));

		BufferedReader in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/test.txt"));
		String resStr = "";
		String str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolComplexBinding;"));
		assertTrue(resStr.contains("import java.util.List;"));
		assertTrue(resStr.contains("import java.util.ArrayList;"));
		assertTrue(resStr.contains("import com.legstar.test.coxb.arrayssm.TableComplexType;"));
		assertTrue(resStr.contains("public class TableComplexTypeWrapperBinding"));
		assertTrue(resStr.contains("extends CArrayComplexBinding {"));
		assertTrue(resStr.contains("private List < TableComplexType > mValueObject;"));
		assertTrue(resStr.contains("super(bindingName, fieldName, TableComplexType.class, null, parentBinding, complexItemBinding);"));
		assertTrue(resStr.contains("setMinOccurs(3);"));
		assertTrue(resStr.contains("setMaxOccurs(3);"));
		assertTrue(resStr.contains("mValueObject = new ArrayList < TableComplexType >();"));
		assertTrue(resStr.contains("mValueObject.add((TableComplexType) getComplexItemBinding()."));
		assertTrue(resStr.contains("getObjectValue(TableComplexType.class));"));

		assertTrue(resStr.contains("public final List < TableComplexType > getTableComplexType() throws HostException {"));
		assertTrue(resStr.contains("return (List < TableComplexType >) getObjectValue(TableComplexType.class);"));
	}

	public void testGenArraysdo() throws Exception {

		com.legstar.test.coxb.arraysdo.ObjectFactory objectFactory
		= new com.legstar.test.coxb.arraysdo.ObjectFactory();

		CComplexReflectBinding ce = new CComplexReflectBinding(
				objectFactory,
				JaxbUtil.loadClass("com.legstar.test.coxb.arraysdo.DfhcommareaType"));

		CoxbGenModel coxbContext = new CoxbGenModel();
		coxbContext.setJaxbPackageName("com.legstar.test.coxb.arraysdo");
		coxbContext.setCoxbPackageName("com.legstar.test.coxb.arraysdo.bind");
		mParameters.put("coxbContext", coxbContext);
		mParameters.put("binding-class-name", "DfhcommareaTypeBinding");

		CodeGenUtil.processTemplate(
				BINDING_GENERATOR_NAME,
				"vlc/coxb-bind-complex.vm",
				"binding", ce,
				mParameters,
				CodeGenUtil.getFile(GEN_SRC_DIR, "test.txt"));

		BufferedReader in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/test.txt"));
		String resStr = "";
		String str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("import java.util.List;"));
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolArrayStringBinding;"));

		assertTrue(resStr.contains("public ICobolZonedDecimalBinding tableSize;"));
		assertTrue(resStr.contains("public ICobolArrayStringBinding tableOdo;"));

		assertTrue(resStr.contains("tableSize = BF.createZonedDecimalBinding(\"TableSize\","));
		assertTrue(resStr.contains("\"TableSize\", Integer.class, this);"));
		assertTrue(resStr.contains("tableSize.setCobolName(\"TABLE-SIZE\");"));
		assertTrue(resStr.contains("tableSize.setIsODOObject(true);"));
		assertTrue(resStr.contains("tableSize.setByteLength(2);"));
		assertTrue(resStr.contains("tableSize.setTotalDigits(2);"));
		assertTrue(resStr.contains("tableOdo = BF.createArrayStringBinding(\"TableOdo\","));
		assertTrue(resStr.contains("\"TableOdo\", String.class, this);"));
		assertTrue(resStr.contains("tableOdo.setCobolName(\"TABLE-ODO\");"));
		assertTrue(resStr.contains("tableOdo.setByteLength(5);"));
		assertTrue(resStr.contains("tableOdo.setMinOccurs(1);"));
		assertTrue(resStr.contains("tableOdo.setMaxOccurs(100);"));
		assertTrue(resStr.contains("tableOdo.setDependingOn(\"TABLE-SIZE\");"));
		assertTrue(resStr.contains("tableSize.setObjectValue(mValueObject.getTableSize());"));
		assertTrue(resStr.contains("setCounterValue(tableOdo.getDependingOn(),"));
		assertTrue(resStr.contains("((List < ? >) mValueObject.getTableOdo()).size());"));

		assertTrue(resStr.contains("mValueObject.setTableSize((Integer) bindingValue);"));
		assertTrue(resStr.contains("mValueObject.getTableOdo().clear();"));
		assertTrue(resStr.contains("mValueObject.getTableOdo().addAll("));
		assertTrue(resStr.contains("(List < String >) bindingValue);"));
	}

	public void testGenRedsimpt() throws Exception {

		com.legstar.test.coxb.redsimpt.ObjectFactory objectFactory
		= new com.legstar.test.coxb.redsimpt.ObjectFactory();

		CComplexReflectBinding ce = new CComplexReflectBinding(
				objectFactory,
				JaxbUtil.loadClass("com.legstar.test.coxb.redsimpt.DfhcommareaType"));

		CoxbGenModel coxbContext = new CoxbGenModel();
		coxbContext.setJaxbPackageName("com.legstar.test.coxb.redsimpt");
		coxbContext.setCoxbPackageName("com.legstar.test.coxb.redsimpt.bind");
		mParameters.put("coxbContext", coxbContext);
		mParameters.put("binding-class-name", "DfhcommareaTypeBinding");

		CodeGenUtil.processTemplate(
				BINDING_GENERATOR_NAME,
				"vlc/coxb-bind-complex.vm",
				"binding", ce,
				mParameters,
				CodeGenUtil.getFile(GEN_SRC_DIR, "test.txt"));

		BufferedReader in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/test.txt"));
		String resStr = "";
		String str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolChoiceBinding;"));
		assertTrue(resStr.contains("public ICobolChoiceBinding cDefinition1Choice;"));
		assertTrue(resStr.contains("cDefinition1Choice = new CDefinition1ChoiceBinding(\"CDefinition1Choice\", this);"));
		assertTrue(resStr.contains("cDefinition1Choice.setCobolName(\"C-DEFINITION-1\");"));
		assertTrue(resStr.contains("cDefinition1Choice.setByteLength(18);"));
		assertTrue(resStr.contains("cDefinition1Choice.setUnmarshalChoiceStrategyClassName("));
		assertTrue(resStr.contains("\"com.legstar.coxb.cust.redsimpt.ChoiceSelector\");"));
	}

	public void testGenRedsimptChoice() throws Exception {

		com.legstar.test.coxb.redsimpt.ObjectFactory objectFactory
		= new com.legstar.test.coxb.redsimpt.ObjectFactory();

		CComplexReflectBinding ce = new CComplexReflectBinding(
				objectFactory,
				JaxbUtil.loadClass("com.legstar.test.coxb.redsimpt.DfhcommareaType"));

		ICobolChoiceBinding cc = (ICobolChoiceBinding) ce.getChildrenList().get(0);

		CoxbGenModel coxbContext = new CoxbGenModel();
		coxbContext.setJaxbPackageName("com.legstar.test.coxb.redsimpt");
		coxbContext.setCoxbPackageName("com.legstar.test.coxb.redsimpt.bind");
		mParameters.put("coxbContext", coxbContext);
		mParameters.put("binding-class-name", "CDefinition1ChoiceBinding");


		CodeGenUtil.processTemplate(
				BINDING_GENERATOR_NAME,
				"vlc/coxb-bind-choice.vm",
				"binding", cc,
				mParameters,
				CodeGenUtil.getFile(GEN_SRC_DIR, "test.txt"));

		BufferedReader in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/test.txt"));
		String resStr = "";
		String str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolComplexBinding;"));
		assertTrue(resStr.contains("import com.legstar.coxb.common.CChoiceBinding;"));
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolZonedDecimalBinding;"));
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolStringBinding;"));
		assertTrue(resStr.contains("public ICobolStringBinding cDefinition1;"));
		assertTrue(resStr.contains("public ICobolZonedDecimalBinding cDefinition2;"));
		assertTrue(resStr.contains("setUnmarshalChoiceStrategyClassName("));
		assertTrue(resStr.contains("public ICobolZonedDecimalBinding cDefinition2;"));
		assertTrue(resStr.contains("\"com.legstar.coxb.cust.redsimpt.ChoiceSelector\");"));
		assertTrue(resStr.contains("cDefinition1 = BF.createStringBinding(\"CDefinition1\","));
		assertTrue(resStr.contains("\"CDefinition1\", String.class, getParentBinding());"));
		assertTrue(resStr.contains("cDefinition1.setCobolName(\"C-DEFINITION-1\");"));
		assertTrue(resStr.contains("cDefinition1.setByteLength(18);"));
		assertTrue(resStr.contains("cDefinition1.setUnmarshalChoiceStrategyClassName("));
		assertTrue(resStr.contains("\"com.legstar.coxb.cust.redsimpt.ChoiceSelector\");"));
		assertTrue(resStr.contains("cDefinition2 = BF.createZonedDecimalBinding(\"CDefinition2\","));
		assertTrue(resStr.contains("\"CDefinition2\", Long.class, getParentBinding());"));
		assertTrue(resStr.contains("cDefinition2.setCobolName(\"C-DEFINITION-2\");"));
		assertTrue(resStr.contains("cDefinition2.setByteLength(18);"));
		assertTrue(resStr.contains("cDefinition2.setTotalDigits(18);"));
		assertTrue(resStr.contains("cDefinition2.setRedefines(\"C-DEFINITION-1\");"));
		assertTrue(resStr.contains("addAlternative(cDefinition1);"));
		assertTrue(resStr.contains("addAlternative(cDefinition2);"));

		assertTrue(resStr.contains("value = getDfhcommareaType().getCDefinition1();"));
		assertTrue(resStr.contains("cDefinition1.setObjectValue(value);"));
		assertTrue(resStr.contains("value = getDfhcommareaType().getCDefinition2();"));
		assertTrue(resStr.contains("cDefinition2.setObjectValue(value);"));

		assertTrue(resStr.contains("getDfhcommareaType().setCDefinition1("));
		assertTrue(resStr.contains("(String) bindingValue);"));
		assertTrue(resStr.contains("getDfhcommareaType().setCDefinition2("));
		assertTrue(resStr.contains("(Long) bindingValue);"));

		assertTrue(resStr.contains("public final DfhcommareaType getDfhcommareaType() throws HostException {"));
		assertTrue(resStr.contains("return ((DfhcommareaTypeBinding) getParentBinding()).getDfhcommareaType();"));
		assertTrue(resStr.contains("return (DfhcommareaType) getParentValueObject();"));
	}

	public void testGenRedsimptChoiceStrategy() throws Exception {

		com.legstar.test.coxb.redsimpt.ObjectFactory objectFactory
		= new com.legstar.test.coxb.redsimpt.ObjectFactory();

		CComplexReflectBinding ce = new CComplexReflectBinding(
				objectFactory,
				JaxbUtil.loadClass("com.legstar.test.coxb.redsimpt.DfhcommareaType"));

		ICobolChoiceBinding cc = (ICobolChoiceBinding) ce.getChildrenList().get(0);

		CoxbGenModel coxbContext = new CoxbGenModel();
		coxbContext.setJaxbPackageName("com.legstar.test.coxb.redsimpt");
		coxbContext.setCoxbPackageName("com.legstar.test.coxb.redsimpt.bind");
		mParameters.put("coxbContext", coxbContext);
		mParameters.put("choice-strategy-type", "Unmarshal");
		mParameters.put("choice-strategy-qualified-class-name", "com.legstar.coxb.cust.redsimpt.ChoiceSelector");

		CodeGenUtil.processTemplate(
				BINDING_GENERATOR_NAME,
				"vlc/coxb-bind-choice-strategy.vm",
				"binding", cc,
				mParameters,
				CodeGenUtil.getFile(GEN_SRC_DIR, "test.txt"));

		BufferedReader in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/test.txt"));
		String resStr = "";
		String str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("package com.legstar.coxb.cust.redsimpt;"));
		assertTrue(resStr.contains("public class ChoiceSelector implements ICobolUnmarshalChoiceStrategy {"));
		assertTrue(resStr.contains("DfhcommareaType valueObject = (DfhcommareaType) choice.getObjectValue(DfhcommareaType.class);"));
		assertTrue(resStr.contains("return choice.getAlternativeByName(\"CDefinition1\");"));
		assertTrue(resStr.contains("return choice.getAlternativeByName(\"CDefinition2\");"));
	}

	public void testGenJvmQueryReply() throws Exception {

		com.legstar.test.coxb.jvmquery.ObjectFactory objectFactory
		= new com.legstar.test.coxb.jvmquery.ObjectFactory();

		CComplexReflectBinding ce = new CComplexReflectBinding(
				objectFactory,
				JaxbUtil.loadClass("com.legstar.test.coxb.jvmquery.JvmQueryReplyType"));

		CoxbGenModel coxbContext = new CoxbGenModel();
		coxbContext.setJaxbPackageName("com.legstar.test.coxb.jvmquery");
		coxbContext.setCoxbPackageName("com.legstar.test.coxb.jvmquery.bind");
		coxbContext.setAlternativePackageName("com.legstar.xsdc.test.cases.jvmquery");
		coxbContext.setAlternativeFactoryName("ObjectFactory");
		
		mParameters.put("coxbContext", coxbContext);
		mParameters.put("binding-class-name", "JvmQueryReplyBinding");

		CodeGenUtil.processTemplate(
				BINDING_GENERATOR_NAME,
				"vlc/coxb-bind-complex.vm",
				"binding", ce,
				mParameters,
				CodeGenUtil.getFile(GEN_SRC_DIR, "test.txt"));

		BufferedReader in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/test.txt"));
		String resStr = "";
		String str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolBinding;"));
		assertTrue(resStr.contains("import com.legstar.coxb.common.CComplexBinding;"));
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolBinaryBinding;"));
		assertTrue(resStr.contains("import com.legstar.coxb.CobolBindingFactory;"));
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolBindingFactory;"));
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolStringBinding;"));
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolArrayStringBinding;"));
		assertTrue(resStr.contains("import java.util.List;"));
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolComplexBinding;"));
		
		assertTrue(resStr.contains("import com.legstar.xsdc.test.cases.jvmquery.JVMQueryReply;"));
		
		assertTrue(resStr.contains("public class JvmQueryReplyBinding"));
		assertTrue(resStr.contains("private JVMQueryReply mValueObject;"));
		assertTrue(resStr.contains("public ICobolBinaryBinding envVarValuesCounter;"));
		assertTrue(resStr.contains("public ICobolStringBinding country;"));
		assertTrue(resStr.contains("public ICobolStringBinding currencySymbol;"));
		assertTrue(resStr.contains("public ICobolArrayStringBinding envVarValues;"));
		assertTrue(resStr.contains("public ICobolStringBinding formattedDate;"));
		assertTrue(resStr.contains("public ICobolStringBinding language;"));

		assertTrue(resStr.contains("public JvmQueryReplyBinding("));
		assertTrue(resStr.contains("final JVMQueryReply valueObject) {"));
		assertTrue(resStr.contains("super(bindingName, fieldName, JVMQueryReply.class, null, parentBinding);"));

		assertTrue(resStr.contains("mValueObject = new JVMQueryReply();"));
		assertTrue(resStr.contains("if (type.equals(JVMQueryReply.class)) {"));
		assertTrue(resStr.contains("if (bindingValue.getClass().equals(JVMQueryReply.class)) {"));
		assertTrue(resStr.contains("mValueObject = (JVMQueryReply) bindingValue;"));
		assertTrue(resStr.contains("public final JVMQueryReply getJVMQueryReply() {"));
	}
}

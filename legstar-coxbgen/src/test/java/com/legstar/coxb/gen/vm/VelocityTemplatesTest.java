package com.legstar.coxb.gen.vm;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.codegen.CodeGenUtil;
import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.gen.CoxbHelper;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;

import junit.framework.TestCase;

public class VelocityTemplatesTest extends TestCase {
	
    /** Logger. */
	private static final Log LOG = LogFactory.getLog(VelocityTemplatesTest.class);
	
	/** Code will be generated here. */
	private static final String GEN_SRC_DIR = "src/test/gen/java";

	public void setUp() throws Exception {
        CodeGenUtil.initVelocity();
       	CodeGenUtil.checkDirectory(GEN_SRC_DIR, true);
	}
	
	public void testGenAllTypes() throws Exception {
		
		com.legstar.test.coxb.lsfilead.ObjectFactory objectFactory
			= new com.legstar.test.coxb.lsfilead.ObjectFactory();
		
    	CComplexReflectBinding ce = new CComplexReflectBinding(
    			objectFactory,
    			Class.forName("com.legstar.test.coxb.alltypes.DfhcommareaType"));
    	
    	Map <String, Object> parameters = new HashMap <String, Object>();
    	parameters.put("jaxb-package", "com.legstar.test.coxb.alltypes");
    	parameters.put("binding-type-package", "com.legstar.test.coxb.alltypes.bind");
    	parameters.put("binding-class-name", "DfhcommareaTypeBinding");
    	CoxbHelper helper = new CoxbHelper();
    	parameters.put("helper", helper);

    	CodeGenUtil.processTemplate("vlc/coxb-bind-complex.vm",
        		"binding", ce,
        		parameters,
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
        assertTrue(resStr.contains("final DfhcommareaType jaxbObject) {"));
        assertTrue(resStr.contains("this(\"\", \"\", null, jaxbObject);"));
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
        assertTrue(resStr.contains("sString.setObjectValue(mJaxbObject.getSString());"));
        assertTrue(resStr.contains("sUshort.setObjectValue(mJaxbObject.getSUshort());"));
        assertTrue(resStr.contains("aDec.setObjectValue(mJaxbObject.getADec());"));
        assertTrue(resStr.contains("aFloat.setObjectValue(mJaxbObject.getAFloat());"));
        assertTrue(resStr.contains("aDouble.setObjectValue(mJaxbObject.getADouble());"));
 
        assertTrue(resStr.contains("mJaxbObject.setSString((String) bindingValue);"));
        assertTrue(resStr.contains("mJaxbObject.getADouble().clear();"));
        assertTrue(resStr.contains("mJaxbObject.getADouble().addAll("));
        assertTrue(resStr.contains("(List < Double >) bindingValue);"));
	}

	public void testGenArrayssm() throws Exception {
		
		com.legstar.test.coxb.arrayssm.ObjectFactory objectFactory
			= new com.legstar.test.coxb.arrayssm.ObjectFactory();
		
    	CComplexReflectBinding ce = new CComplexReflectBinding(
    			objectFactory,
    			Class.forName("com.legstar.test.coxb.arrayssm.DfhcommareaType"));
    	
    	Map <String, Object> parameters = new HashMap <String, Object>();
    	parameters.put("jaxb-package", "com.legstar.test.coxb.arrayssm");
    	parameters.put("binding-type-package", "com.legstar.test.coxb.arrayssm.bind");
    	parameters.put("binding-class-name", "DfhcommareaTypeBinding");
    	CoxbHelper helper = new CoxbHelper();
    	parameters.put("helper", helper);

    	CodeGenUtil.processTemplate("vlc/coxb-bind-complex.vm",
        		"binding", ce,
        		parameters,
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

        assertTrue(resStr.contains("mJaxbObject.getTableComplex().clear();"));
        assertTrue(resStr.contains("mJaxbObject.getTableComplex().addAll("));
        assertTrue(resStr.contains("(List < TableComplexType >) bindingValue);"));
	}

	public void testGenArrayssmWrapper() throws Exception {
		
		com.legstar.test.coxb.arrayssm.ObjectFactory objectFactory
			= new com.legstar.test.coxb.arrayssm.ObjectFactory();
		
    	CComplexReflectBinding ce = new CComplexReflectBinding(
    			objectFactory,
    			Class.forName("com.legstar.test.coxb.arrayssm.DfhcommareaType"));
    	
    	ICobolArrayComplexBinding ca = (ICobolArrayComplexBinding) ce.getChildrenList().get(1);
    	
    	Map <String, Object> parameters = new HashMap <String, Object>();
    	parameters.put("jaxb-package", "com.legstar.test.coxb.arrayssm");
    	parameters.put("binding-type-package", "com.legstar.test.coxb.arrayssm.bind");
    	parameters.put("binding-class-name", "TableComplexTypeWrapperBinding");
    	CoxbHelper helper = new CoxbHelper();
    	parameters.put("helper", helper);

    	CodeGenUtil.processTemplate("vlc/coxb-bind-complex-array.vm",
        		"binding", ca,
        		parameters,
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
        assertTrue(resStr.contains("private List < TableComplexType > mJaxbObject;"));
        assertTrue(resStr.contains("super(bindingName, jaxbName, TableComplexType.class, null, parentBinding, complexItemBinding);"));
        assertTrue(resStr.contains("setMinOccurs(3);"));
        assertTrue(resStr.contains("setMaxOccurs(3);"));
        assertTrue(resStr.contains("mJaxbObject = new ArrayList < TableComplexType >();"));
        assertTrue(resStr.contains("mJaxbObject.add((TableComplexType) getComplexItemBinding()."));
        assertTrue(resStr.contains("getObjectValue(TableComplexType.class));"));
        
        assertTrue(resStr.contains("public final List < TableComplexType > getTableComplexType() throws HostException {"));
        assertTrue(resStr.contains("return (List < TableComplexType >) getObjectValue(TableComplexType.class);"));
	}

	public void testGenArraysdo() throws Exception {
		
		com.legstar.test.coxb.arraysdo.ObjectFactory objectFactory
			= new com.legstar.test.coxb.arraysdo.ObjectFactory();
		
    	CComplexReflectBinding ce = new CComplexReflectBinding(
    			objectFactory,
    			Class.forName("com.legstar.test.coxb.arraysdo.DfhcommareaType"));
    	
    	Map <String, Object> parameters = new HashMap <String, Object>();
    	parameters.put("jaxb-package", "com.legstar.test.coxb.arraysdo");
    	parameters.put("binding-type-package", "com.legstar.test.coxb.arraysdo.bind");
    	parameters.put("binding-class-name", "DfhcommareaTypeBinding");
    	CoxbHelper helper = new CoxbHelper();
    	parameters.put("helper", helper);

    	CodeGenUtil.processTemplate("vlc/coxb-bind-complex.vm",
        		"binding", ce,
        		parameters,
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
        assertTrue(resStr.contains("tableSize.setObjectValue(mJaxbObject.getTableSize());"));
        assertTrue(resStr.contains("setCounterValue(tableOdo.getDependingOn(),"));
        assertTrue(resStr.contains("((List < ? >) mJaxbObject.getTableOdo()).size());"));

        assertTrue(resStr.contains("mJaxbObject.setTableSize((Integer) bindingValue);"));
        assertTrue(resStr.contains("mJaxbObject.getTableOdo().clear();"));
        assertTrue(resStr.contains("mJaxbObject.getTableOdo().addAll("));
        assertTrue(resStr.contains("(List < String >) bindingValue);"));
	}

	public void testGenRedsimpt() throws Exception {
		
		com.legstar.test.coxb.redsimpt.ObjectFactory objectFactory
			= new com.legstar.test.coxb.redsimpt.ObjectFactory();
		
    	CComplexReflectBinding ce = new CComplexReflectBinding(
    			objectFactory,
    			Class.forName("com.legstar.test.coxb.redsimpt.DfhcommareaType"));
    	
    	Map <String, Object> parameters = new HashMap <String, Object>();
    	parameters.put("jaxb-package", "com.legstar.test.coxb.redsimpt");
    	parameters.put("binding-type-package", "com.legstar.test.coxb.redsimpt.bind");
    	parameters.put("binding-class-name", "DfhcommareaTypeBinding");
    	CoxbHelper helper = new CoxbHelper();
    	parameters.put("helper", helper);

    	CodeGenUtil.processTemplate("vlc/coxb-bind-complex.vm",
        		"binding", ce,
        		parameters,
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
    			Class.forName("com.legstar.test.coxb.redsimpt.DfhcommareaType"));
    	
    	ICobolChoiceBinding cc = (ICobolChoiceBinding) ce.getChildrenList().get(0);

    	Map <String, Object> parameters = new HashMap <String, Object>();
    	parameters.put("jaxb-package", "com.legstar.test.coxb.redsimpt");
    	parameters.put("binding-type-package", "com.legstar.test.coxb.redsimpt.bind");
    	parameters.put("binding-class-name", "CDefinition1ChoiceBinding");
    	CoxbHelper helper = new CoxbHelper();
    	parameters.put("helper", helper);

    	CodeGenUtil.processTemplate("vlc/coxb-bind-choice.vm",
        		"binding", cc,
        		parameters,
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

        assertTrue(resStr.contains("getDfhcommareaType().setCDefinition1("));
        assertTrue(resStr.contains("(String) bindingValue);"));
        assertTrue(resStr.contains("getDfhcommareaType().setCDefinition2("));
        assertTrue(resStr.contains("(Long) bindingValue);"));

        assertTrue(resStr.contains("public final DfhcommareaType getDfhcommareaType() throws HostException {"));
        assertTrue(resStr.contains("return (DfhcommareaType) getParentJaxbObject();"));
	}

	public void testGenRedsimptChoiceStrategy() throws Exception {
		
		com.legstar.test.coxb.redsimpt.ObjectFactory objectFactory
			= new com.legstar.test.coxb.redsimpt.ObjectFactory();
		
		CComplexReflectBinding ce = new CComplexReflectBinding(
				objectFactory,
				Class.forName("com.legstar.test.coxb.redsimpt.DfhcommareaType"));
		
		ICobolChoiceBinding cc = (ICobolChoiceBinding) ce.getChildrenList().get(0);
	
		Map <String, Object> parameters = new HashMap <String, Object>();
    	parameters.put("binding-type-package", "com.legstar.test.coxb.redsimpt.bind");
    	parameters.put("choice-strategy-type", "Unmarshal");
		parameters.put("choice-strategy-qualified-class-name", "com.legstar.coxb.cust.redsimpt.ChoiceSelector");
		CoxbHelper helper = new CoxbHelper();
		parameters.put("helper", helper);
	
		CodeGenUtil.processTemplate("vlc/coxb-bind-choice-strategy.vm",
	    		"binding", cc,
	    		parameters,
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
	    assertTrue(resStr.contains("DfhcommareaType jaxbo = (DfhcommareaType) choice.getObjectValue(DfhcommareaType.class);"));
	    assertTrue(resStr.contains("return choice.getAlternativeByName(\"CDefinition1\");"));
	    assertTrue(resStr.contains("return choice.getAlternativeByName(\"CDefinition2\");"));
	}
}

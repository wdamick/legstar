package com.legstar.coxb.gen;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.ICobolArrayComplexBinding;
import com.legstar.coxb.ICobolChoiceBinding;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;

import junit.framework.TestCase;

public class CoxbGenWriterTest extends TestCase {

	/** Code will be generated here. */
	private static final String GEN_SRC_DIR = "src/test/gen/java";

	/** Logger. */
	private static final Log LOG = LogFactory.getLog(CoxbGenWriterTest.class);

	public void testGenAlltypes() throws Exception {
		com.legstar.test.coxb.lsfilead.ObjectFactory objectFactory
		= new com.legstar.test.coxb.lsfilead.ObjectFactory();

		CComplexReflectBinding ce = new CComplexReflectBinding(
				objectFactory,
				Class.forName("com.legstar.test.coxb.alltypes.DfhcommareaType"));

		CoxbGenContext coxbGenContext = new CoxbGenContext();
		coxbGenContext.setTargetDir(new File(GEN_SRC_DIR));
		coxbGenContext.setJaxbPackageName("com.legstar.test.coxb.alltypes");
		coxbGenContext.setCoxbPackageName("com.legstar.test.coxb.alltypes.bind");
		
		CoxbGenWriter writer = new CoxbGenWriter(coxbGenContext);

		writer.write(ce);

		BufferedReader in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/com/legstar/test/coxb/alltypes/bind/DfhcommareaTypeBinding.java"));
		String resStr = "";
		String str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
		assertTrue(resStr.contains("import com.legstar.coxb.ICobolStringBinding;"));
	}

	public void testGenRedsimpt() throws Exception {
		com.legstar.test.coxb.redsimpt.ObjectFactory objectFactory
		= new com.legstar.test.coxb.redsimpt.ObjectFactory();
	
		CComplexReflectBinding ce = new CComplexReflectBinding(
				objectFactory,
				Class.forName("com.legstar.test.coxb.redsimpt.DfhcommareaType"));
		
		ICobolChoiceBinding cc = (ICobolChoiceBinding) ce.getChildrenList().get(0);

		CoxbGenContext coxbGenContext = new CoxbGenContext();
		coxbGenContext.setTargetDir(new File(GEN_SRC_DIR));
		coxbGenContext.setJaxbPackageName("com.legstar.test.coxb.redsimpt");
		coxbGenContext.setCoxbPackageName("com.legstar.test.coxb.redsimpt.bind");
		
		CoxbGenWriter writer = new CoxbGenWriter(coxbGenContext);

		writer.write(cc);

		BufferedReader in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/com/legstar/test/coxb/redsimpt/bind/CDefinition1ChoiceBinding.java"));
		String resStr = "";
		String str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
        assertTrue(resStr.contains("import com.legstar.coxb.ICobolComplexBinding;"));
	}

	public void testGenArrayssm() throws Exception {
		com.legstar.test.coxb.arrayssm.ObjectFactory objectFactory
			= new com.legstar.test.coxb.arrayssm.ObjectFactory();
		
		CComplexReflectBinding ce = new CComplexReflectBinding(
				objectFactory,
				Class.forName("com.legstar.test.coxb.arrayssm.DfhcommareaType"));
		
		ICobolArrayComplexBinding ca = (ICobolArrayComplexBinding) ce.getChildrenList().get(1);

		CoxbGenContext coxbGenContext = new CoxbGenContext();
		coxbGenContext.setTargetDir(new File(GEN_SRC_DIR));
		coxbGenContext.setJaxbPackageName("com.legstar.test.coxb.arrayssm");
		coxbGenContext.setCoxbPackageName("com.legstar.test.coxb.arrayssm.bind");
		
		CoxbGenWriter writer = new CoxbGenWriter(coxbGenContext);

		writer.write(ca);

		BufferedReader in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/com/legstar/test/coxb/arrayssm/bind/TableComplexTypeWrapperBinding.java"));
		String resStr = "";
		String str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
        assertTrue(resStr.contains("import com.legstar.coxb.common.CArrayComplexBinding;"));
	}

	public void testGenChoiceStrategy() throws Exception {
		com.legstar.test.coxb.redsimpt.ObjectFactory objectFactory
			= new com.legstar.test.coxb.redsimpt.ObjectFactory();
			
		CComplexReflectBinding ce = new CComplexReflectBinding(
				objectFactory,
				Class.forName("com.legstar.test.coxb.redsimpt.DfhcommareaType"));
		
		ICobolChoiceBinding cc = (ICobolChoiceBinding) ce.getChildrenList().get(0);

		CoxbGenContext coxbGenContext = new CoxbGenContext();
		coxbGenContext.setTargetDir(new File(GEN_SRC_DIR));
		coxbGenContext.setJaxbPackageName("com.legstar.test.coxb.redsimpt");
		coxbGenContext.setCoxbPackageName("com.legstar.test.coxb.redsimpt.bind");
		
		CoxbGenWriter writer = new CoxbGenWriter(coxbGenContext);

		/* Do it twice to check the backup mechanism */
		writer.write(cc, "Unmarshal", "com.legstar.coxb.cust.redsimpt.ChoiceSelector");
		writer.write(cc, "Unmarshal", "com.legstar.coxb.cust.redsimpt.ChoiceSelector");

		BufferedReader in = new BufferedReader(new FileReader(GEN_SRC_DIR + "/com/legstar/coxb/cust/redsimpt/ChoiceSelector.java.new"));
		String resStr = "";
		String str = in.readLine();
		while (str != null) {
			LOG.debug(str);
			resStr += str;
			str = in.readLine();
		}
		in.close();
        assertTrue(resStr.contains("public class ChoiceSelector implements ICobolUnmarshalChoiceStrategy {"));
	}
}

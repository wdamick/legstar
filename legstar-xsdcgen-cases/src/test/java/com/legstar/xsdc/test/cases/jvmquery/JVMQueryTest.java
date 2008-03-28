package com.legstar.xsdc.test.cases.jvmquery;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import junit.framework.TestCase;

public class JVMQueryTest extends TestCase {
	
	public void testQuery() throws Exception {
		Locale.setDefault(new Locale("jp", "JP"));
		List <String> envVarNames = new ArrayList <String> ();
		JVMQueryRequest request = new JVMQueryRequest();
		request.setEnvVarNames(envVarNames);
		JVMQuery query = new JVMQuery();
		JVMQueryReply reply = query.queryJvm(request);
		assertEquals("Japan", reply.getCountry());
		assertEquals("jp", reply.getLanguage());
		assertEquals("JPY", reply.getCurrencySymbol());
		assertTrue(reply.getFormattedDate().length() > 0);
		assertEquals(0, reply.getEnvVarValues().size());
		
		envVarNames.add("JAVA_HOME");
		reply = query.queryJvm(request);
		assertEquals(1, reply.getEnvVarValues().size());
		assertTrue(reply.getEnvVarValues().get(0).contains("jre")
				|| reply.getEnvVarValues().get(0).contains("jdk"));
	}

}

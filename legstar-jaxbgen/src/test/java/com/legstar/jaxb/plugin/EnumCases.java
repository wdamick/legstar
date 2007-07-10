package com.legstar.jaxb.plugin;

import java.io.File;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;

import junit.framework.TestCase;

public class EnumCases extends TestCase {
	private static final String GEN_SRC_DIR = "src/test/gen/java";
	
	@SuppressWarnings("unchecked")
	public void testGetSetEnum() throws Exception {
		/* Load the generated class */
		File dir = new File(GEN_SRC_DIR);
		URI loc = dir.toURI();
		URL[] ua = {loc.toURL()};
		URLClassLoader cl = new URLClassLoader( ua);
		Class clazzSearchRequestType = cl.loadClass("com.legstar.test.coxb.enumvar.SearchRequestType");
		Object searchRequest = clazzSearchRequestType.newInstance();
		
		/* Create an Enum type with a value */
		Class clazzSafeSearchOptionsType = cl.loadClass("com.legstar.test.coxb.enumvar.SafeSearchOptionsType");
		Field[] fields = clazzSafeSearchOptionsType.getDeclaredFields();
		for (Field field : fields) {
			System.out.println(field.getName());
		}
//		Class[] param0 = {String.class};
//		Method setValue = clazz2.getMethod("fromValue", param0);
//		Object value = setValue.invoke(arg0, arg1);
//		Class[] param2 = {String.class};
//		Constructor constructor = clazz2.getConstructor(param2);
//		Object type2 = constructor.newInstance(new Object[] {"Strict"});
		
		/* Create a new enum type with a value */
        Method getValueMethod = clazzSafeSearchOptionsType.getMethod("value", (Class[]) null);
        Method fromValueMethod = clazzSafeSearchOptionsType.getMethod("fromValue", new Class[] {getValueMethod.getReturnType()});
        Object safeSearchOption = fromValueMethod.invoke(null, new Object [] { "Strict" });

        /* Get the value of an Enum */
        getValueMethod.invoke(safeSearchOption, (Object[]) null);
        
		/* Set the Enum value*/
		Class[] param = {clazzSafeSearchOptionsType};
		String setterName = "setSafeSearch";
		Method setter =
			searchRequest.getClass().getMethod(setterName, param);
		setter.invoke(searchRequest, safeSearchOption);

		String getterName = "getSafeSearch";
		Method getter = searchRequest.getClass().getMethod(getterName);
		Object result = getter.invoke(searchRequest);
		assertEquals("Strict", getValueMethod.invoke(result, (Object[]) null));
	}
	

}

package com.legstar.cixs.gen.model.util;

import com.legstar.cixs.model.util.ModelUtil;

import junit.framework.TestCase;

public class ModelUtilTest extends TestCase {

	/**
	    * Check that class normalization works.
	    * @throws Exception if test fails
	    */
	  public final void testClassNormalization() throws Exception {
	       assertEquals(null, ModelUtil.classNormalize(null));
	       assertEquals("A", ModelUtil.classNormalize("a"));
	       assertEquals("Abc", ModelUtil.classNormalize("abc"));
	   }

	  public final void testFieldNameFromPropertyName() throws Exception {
	       assertEquals(null, ModelUtil.fieldNameFromPropertyName(null));
	       assertEquals("a", ModelUtil.fieldNameFromPropertyName("A"));
	       assertEquals("abc", ModelUtil.fieldNameFromPropertyName("Abc"));
	  }

	  public final void testPropertyNameFromFieldName() throws Exception {
	       assertEquals(null, ModelUtil.propertyNameFromFieldName(null));
	       assertEquals("A", ModelUtil.propertyNameFromFieldName("a"));
	       assertEquals("Abc", ModelUtil.propertyNameFromFieldName("abc"));
	  }

	  public final void testPropertyNameFromJaxbType() throws Exception {
	       assertEquals(null, ModelUtil.propertyNameFromJaxbType(null));
	       assertEquals("A", ModelUtil.propertyNameFromJaxbType("A"));
	       assertEquals("A", ModelUtil.propertyNameFromJaxbType("AType"));
	  }
}

/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/
package com.legstar.schemagen.test;

import junit.framework.TestCase;
import com.legstar.schemagen.COXBSchemaGenerator;
import java.net.URI;
import java.net.URISyntaxException;

/**
 * Deriving package names from namespaces cases.
 */
public class PackageFromURITestCase extends TestCase {
	
	/**
	 * Opaque URI.
	 * @throws URISyntaxException if URI not supported
	 */
	public final void testPackageFromURI1() throws URISyntaxException {
		
		try {
			URI namespaceURI1 = new URI("mailto:java-net@java.sun.com");
			String pkg = COXBSchemaGenerator.packageFromURI(namespaceURI1);
			assertEquals("", pkg);
		
		} catch (Exception e) {
			fail("opaque URI test failed " + e.getMessage());
		}
	}

	/**
	 * Absolute URI.
	 * @throws URISyntaxException if URI not supported
	 */
	public final void testPackageFromURI2() throws URISyntaxException {
		
		try {
			URI namespaceURI1 = new URI("http://java.sun.com/j2se/1.3/");
			String pkg = COXBSchemaGenerator.packageFromURI(namespaceURI1);
			assertEquals("com.sun.java.j2se.1.3", pkg);
		
		} catch (Exception e) {
			fail("hierarchical, absolute,  URI test failed");
		}
	}
	
	/**
	 * Relative URI.
	 * @throws URISyntaxException if URI not supported
	 */
	public final void testPackageFromURI3() throws URISyntaxException {
		
		try {
			URI namespaceURI1 = new URI(
					"docs/guide/collections/designfaq.html#28");
			String pkg = COXBSchemaGenerator.packageFromURI(namespaceURI1);
			assertEquals("docs.guide.collections.designfaq.html.28", pkg);
		
		} catch (Exception e) {
			fail("hierarchical, relative,  URI test failed");
		}
	}
	
	/**
	 * Relative URI with back references.
	 * @throws URISyntaxException if URI not supported
	 */
	public final void testPackageFromURI4() throws URISyntaxException {
		
		try {
			URI namespaceURI1 = new URI(
					"../../../demo/jfc/SwingSet2/src/SwingSet2.java");
			String pkg = COXBSchemaGenerator.packageFromURI(namespaceURI1);
			assertEquals("demo.jfc.swingset2.src.swingset2.java", pkg);
		
		} catch (Exception e) {
			fail("hierarchical, relative, with trimming of .  URI test failed");
		}
	}

	/**
	 * URI with invalid character.
	 * @throws URISyntaxException if URI not supported
	 */
	public final void testPackageFromURI5() throws URISyntaxException {
		
		try {
			URI namespaceURI1 = new URI("file:///~/calendar ");
			String pkg = COXBSchemaGenerator.packageFromURI(namespaceURI1);
			fail("Illegal character test failed " + pkg);
		
		} catch (Exception e) {
			assertEquals(
				"Illegal character in path at index 18: file:///~/calendar ",
				e.getMessage());
		}
	}
}

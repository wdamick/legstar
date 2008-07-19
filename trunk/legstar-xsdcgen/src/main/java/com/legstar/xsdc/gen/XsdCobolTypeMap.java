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
package com.legstar.xsdc.gen;

import java.util.HashMap;

import javax.xml.namespace.QName;

import com.legstar.coxb.CobolType;

/**
 * The type map associates XML Schema types with Cobol data items type
 * candidates. This table is used as a hint and the actual Cobol data
 * type might defer depending on other XML schema properties such as
 * patterns for instance.
 */
public class XsdCobolTypeMap extends HashMap < QName, CobolType > {

	/** Unique serial ID. */
	private static final long serialVersionUID = 8249584897338839375L;

	/** XML Schema namespace. */
	private static final String XSD_NS = "http://www.w3.org/2001/XMLSchema";
	
	/** Creates the type mapping. */
	public XsdCobolTypeMap() {
		super();
		put(new QName(XSD_NS, "string"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "boolean"), CobolType.BINARY_ITEM);
		put(new QName(XSD_NS, "decimal"), CobolType.PACKED_DECIMAL_ITEM);
		put(new QName(XSD_NS, "float"), CobolType.SINGLE_FLOAT_ITEM);
		put(new QName(XSD_NS, "double"), CobolType.DOUBLE_FLOAT_ITEM);
		put(new QName(XSD_NS, "date"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "dateTime"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "duration"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "gDay"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "gMonth"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "gMonthDay"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "gYear"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "base64Binary"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "hexBinary"), CobolType.OCTET_STREAM_ITEM);
		put(new QName(XSD_NS, "QName"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "NOTATION"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "anyURI"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "gYearMonth"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "time"), CobolType.ALPHANUMERIC_ITEM);

		put(new QName(XSD_NS, "positiveInteger"), CobolType.BINARY_ITEM);
		put(new QName(XSD_NS, "negativeInteger"), CobolType.BINARY_ITEM);
		put(new QName(XSD_NS, "nonNegativeInteger"), CobolType.BINARY_ITEM);
		put(new QName(XSD_NS, "nonPositiveInteger"), CobolType.BINARY_ITEM);
		put(new QName(XSD_NS, "unsignedShort"), CobolType.BINARY_ITEM);
		put(new QName(XSD_NS, "unsignedLong"), CobolType.BINARY_ITEM);
		put(new QName(XSD_NS, "unsignedInt"), CobolType.BINARY_ITEM);
		put(new QName(XSD_NS, "unsignedByte"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "long"), CobolType.BINARY_ITEM);
		put(new QName(XSD_NS, "short"), CobolType.BINARY_ITEM);
		put(new QName(XSD_NS, "int"), CobolType.BINARY_ITEM);
		put(new QName(XSD_NS, "byte"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "token"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "language"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "NMTOKEN"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "NMTOKENS"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "ID"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "IDREF"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "ENTITY"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "ENTITIES"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "Name"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "NCName"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "normalizedString"), CobolType.ALPHANUMERIC_ITEM);
		put(new QName(XSD_NS, "integer"), CobolType.BINARY_ITEM);
	}
}

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
package com.legstar.xsdc.test.cases.jvmquery;

public class JVMQueryException extends Exception {

	/** serialVersionUID. */
	private static final long serialVersionUID = 7635942844695876914L;

	public JVMQueryException(final String string) {
		super(string);
	}
	public JVMQueryException(final Exception e) {
		super(e);
	}

}

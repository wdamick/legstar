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
package com.legstar.eclipse.plugin.cixsgen.wizards;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;

/**
 * The "New" wizard page allows setting the container for the new file as well
 * as the file name.
 */

public class NewWSWizardPage extends WizardNewFileCreationPage {

	/** Page name. */
	private static final String PAGE_NAME =
		"NewWSWizardPage";
	
	/** Page description text. */
	private static final String PAGE_TITLE =
		"CIXS Generation";
	
	/** Page description text. */
	private static final String PAGE_DESC =
		"Generate CIXS Web Service Endpoint";
	
	/** New Web Service name label. */
	private static final String NEW_WS_NAME =
		"New Web Service name:";
	
	 /**
	 * Constructor for NewWSWizardPage.
	 * 
	 * @param selection current selection
	 */
	public NewWSWizardPage(
			final IStructuredSelection selection) {
		super(PAGE_NAME, selection);
		setTitle(PAGE_TITLE);
		setDescription(PAGE_DESC);
	}

	/**
	 * @see org.eclipse.ui.dialogs.WizardNewFileCreationPage#getNewFileLabel()
     * @return the label to display in the file name specification visual
     *     component group
	 */
	@Override
	protected final String getNewFileLabel() {
		return NEW_WS_NAME;
	}

	/**
	 * @return the container name
	 */
	public final String getContainerName() {
		return getContainerFullPath().toOSString();
	}

	/**
	 * @return the web service name
	 */
	public final String getWSName() {
		return getFileName();
	}

}

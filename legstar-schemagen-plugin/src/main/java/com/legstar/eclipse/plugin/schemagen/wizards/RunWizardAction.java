/*******************************************************************************
 *  LegStar legacy Web-enablement .
 *  Copyright (C) 2007 LegSem
 *  
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *  
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *   
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301  USA
 *  
 *******************************************************************************/
package com.legstar.eclipse.plugin.schemagen.wizards;

import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.jface.dialogs.IPageChangedListener;
import org.eclipse.jface.dialogs.PageChangedEvent;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.core.runtime.CoreException;

/**
 * Creates a Wizard environment in response to an action.
 */
public class RunWizardAction  extends Action 
	implements IWorkbenchWindowActionDelegate, IPageChangedListener {

	/**
	 * Constructor.
	 */
	public RunWizardAction() {
		super();
	}
	
	/** {@inheritDoc}. */
	public void init(final IWorkbenchWindow window) {
	}

	/** Called when the action is discarded. */
	public void dispose() {
	}

	/** {@inheritDoc}. */
	public final void run(final IAction action) {
		SchemaGenWizard wizard;
		try {
			wizard = new SchemaGenWizard();
		} catch (CoreException e) {
			e.printStackTrace();
			throw new RuntimeException(e);
		}
		Shell shell =
			PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		WizardDialog dialog = new WizardDialog(shell, wizard);
		dialog.addPageChangedListener(this);
		dialog.create();
		dialog.open();
	}

	/** {@inheritDoc}. */
	public final void selectionChanged(
			final IAction action, final ISelection selection) {
	}

	/**
	 * @see org.eclipse.jface.dialogs.IPageChangedListener#pageChanged(
	 * org.eclipse.jface.dialogs.PageChangedEvent)
	 * @param event
	 *            event object describing the change
	 */
	public final void pageChanged(
			final PageChangedEvent event) {
		/* When switching to XSD page, initialize dialog values from
		 * data gathered on the import Cobol page. */
		if (event.getSelectedPage() instanceof NewXSDWizardPage) {
			NewXSDWizardPage xsdPage =
				(NewXSDWizardPage) event.getSelectedPage();
			ImportCobolWizardPage cobolPage =
				(ImportCobolWizardPage) xsdPage.getWizard().
				getPreviousPage(xsdPage);
				xsdPage.initFieldsFromCobolPage(cobolPage);
		}

	}

}

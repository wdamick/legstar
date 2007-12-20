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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import com.legstar.eclipse.plugin.schemagen.Activator;


/**
 * The is a new file type of wizard used to create an XML schema file.
 */
/**
 * @author Fady
 *
 */
public class ImportCobolWizardPage extends WizardNewFileCreationPage {
	
	/** Field used to locate the cobol file. */
	private FileFieldEditor mEditor;
	
	/** Page description text. */
	private static final String PAGE_TITLE =
		"Import COBOL source file";
	
	/** Page description text. */
	private static final String PAGE_DESC =
		"Import a COBOL source file into the workspace";
	
	/** Page image location. */
	private static final String PAGE_IMG = "icons/legsemlogo.jpg";
	
	/** Field editor preference store ID. */
	private static final String FIELD_EDITOR_PREF_ID = "fileSelect";
	
	/** Field editor label text. */
	private static final String FIELD_EDITOR_LABEL = "Select COBOL File:";
	
	/** New file name label text. */
	private static final String NEW_NAME_LABEL = "New File Name:";
	
	/** Error message received when file name already exist in workspace. */
	private static final String MSG_ALREADY_EXIST =
		"The same name already exists.";
	
	/**
	 * Constructor.
	 * @param selection the current selection
	 */
	public ImportCobolWizardPage(final IStructuredSelection selection) {
		super(PAGE_TITLE, selection);
		setTitle(PAGE_TITLE);
		setDescription(PAGE_DESC);
		ImageDescriptor image =
            AbstractUIPlugin.
                imageDescriptorFromPlugin(Activator.PLUGIN_ID, PAGE_IMG);
        setImageDescriptor(image);
	}

	/**
	 * @see org.eclipse.ui.dialogs.WizardNewFileCreationPage#
	 * createAdvancedControls(org.eclipse.swt.widgets.Composite)
     * @param parent the parent composite
	 */	
	@Override
	protected final void createAdvancedControls(
			final Composite parent) {
		Composite fileSelectionArea = new Composite(parent, SWT.NONE);
		GridData fileSelectionData = new GridData(GridData.GRAB_HORIZONTAL
				| GridData.FILL_HORIZONTAL);
		fileSelectionArea.setLayoutData(fileSelectionData);

		GridLayout fileSelectionLayout = new GridLayout();
		fileSelectionLayout.numColumns = 3;
		fileSelectionLayout.makeColumnsEqualWidth = false;
		fileSelectionLayout.marginWidth = 0;
		fileSelectionLayout.marginHeight = 0;
		fileSelectionArea.setLayout(fileSelectionLayout);
		
		mEditor = new FileFieldEditor(FIELD_EDITOR_PREF_ID,
				FIELD_EDITOR_LABEL, fileSelectionArea); 
		mEditor.getTextControl(fileSelectionArea).addModifyListener(
				new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				IPath path = new Path(
						ImportCobolWizardPage.this.mEditor.getStringValue());
				setFileName(path.lastSegment());
			}
		});
		
		String[] extensions = new String[] { "*.*" };
		mEditor.setFileExtensions(extensions);
		fileSelectionArea.moveAbove(null);

	}
	
    /**
     * @see org.eclipse.ui.dialogs.WizardNewFileCreationPage#handleEvent(
     * org.eclipse.swt.widgets.Event)
     * @param event the event
     */
	@Override
    public final void handleEvent(final Event event) {
    	boolean valid = validatePage();
    	if (!valid && getErrorMessage() != null) {
    		/* There is no way we can force the resourcegroup to accept
    		 * already existing files so we consider container and file
    		 * to be valid if the file already exist. */
    		if (getErrorMessage().compareTo(MSG_ALREADY_EXIST) == 0) {
    	        setPageComplete(true);
    	        setErrorMessage(null);
    	        return;
    		}
    	}
        setPageComplete(valid);
    }
    
	 /** 
	 * @see org.eclipse.ui.dialogs.WizardNewFileCreationPage#createLinkTarget()
	 */
	@Override
	protected void createLinkTarget() {
	}

	/**
	 * @see org.eclipse.ui.dialogs.WizardNewFileCreationPage#
	 * getInitialContents()
     * @return initial contents to be given to new file resource instances
	 */
	@Override
	protected final InputStream getInitialContents() {
		try {
			return new FileInputStream(new File(mEditor.getStringValue()));
		} catch (FileNotFoundException e) {
			return null;
		}
	}

	/**
	 * @see org.eclipse.ui.dialogs.WizardNewFileCreationPage#getNewFileLabel()
     * @return the label to display in the file name specification visual
     *     component group
	 */
	@Override
	protected final String getNewFileLabel() {
		return NEW_NAME_LABEL;
	}

	/**
	 * @see org.eclipse.ui.dialogs.WizardNewFileCreationPage#
	 * validateLinkedResource()
     * @return IStatus validation result from the CreateLinkedResourceGroup
	 */
	@Override
	protected final IStatus validateLinkedResource() {
		return new Status(
				IStatus.OK, Activator.PLUGIN_ID, IStatus.OK, "", null);
	}

	/**
	 * @return the editor.
	 */
	public final FileFieldEditor getEditor() {
		return mEditor;
	}
	
	/**
	 * @return the cobol module name.
	 */
	public final String getCobolName() {
		int dotLoc = getFileName().lastIndexOf('.');
		if (dotLoc != -1) {
			return getFileName().substring(0, dotLoc);
		}
		return getFileName();
	}
	
}

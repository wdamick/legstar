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
package com.legstar.eclipse.plugin.cixsmap.dialogs;

import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;

import com.legstar.cixs.gen.model.CixsStructure;
import com.legstar.eclipse.plugin.cixsmap.Messages;
import com.legstar.eclipse.plugin.common.dialogs.AbstractDialog;

/**
 * This control displays a table and add/modify/delete buttons for
 * structures editing.
 */
public class StructuresTableComposite extends Composite {

	/** Used in error messages. */
	private String mPluginID;

	/** Add new operation. */
	private Button mAddButton;
	
	/** Modify an operation. */
	private Button mModifyButton;

	/** Delete an operation. */
	private Button mDeleteButton;

	/** SWT Table holding operations attributes. */
	private Table mStructuresTable = null;

	/** The collection of structures. */
	private List < CixsStructure > mStructures;
	
	/** The legacy mapping file. */
	private IFile mMappingFile;
	
	/**
	 * Creates a composite control with a table for structures handling.
	 * @param pluginID the current plugin ID
	 * @param parent the parent composite
	 * @param style an additional style
	 * @param mappingFile the current mapping file
	 * @param structures a set if structures to edit
	 */
	public StructuresTableComposite(
			final String pluginID,
			final Composite parent,
			final int style,
			final IFile mappingFile,
			final List < CixsStructure > structures) {
		super(parent, style);
		mPluginID = pluginID;
		mMappingFile = mappingFile;
		mStructures = structures;
		GridLayout layout = new GridLayout(2, false);
		layout.marginWidth = 0;
		layout.marginHeight = 0;
		this.setLayout(layout);
		createControls(this);
		loadStructures();
	}
	
	/**
	 * Create the inner widgets.
	 * @param parent the parent widget	
	 */
	private void createControls(final Composite parent) {

		final Composite btnContainer = new Composite(parent, SWT.NULL);
		GridLayout layout2 = new GridLayout(1, false);
		layout2.marginWidth = 0;
		layout2.marginHeight = 0;
		btnContainer.setLayout(layout2);
		GridData gridData = new GridData(GridData.FILL_BOTH);
		btnContainer.setLayoutData(gridData);

		mAddButton = AbstractDialog.createButton(btnContainer,
				Messages.add_button_label);
		mAddButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				handleAdd();
			}
		});
		mAddButton.setEnabled(true); // Add is always available
		
		mModifyButton = AbstractDialog.createButton(btnContainer,
				Messages.edit_button_label);
		mModifyButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				handleModify();
			}
		});
		mDeleteButton = AbstractDialog.createButton(btnContainer,
				Messages.delete_button_label);
		mDeleteButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				handleDelete();
			}
		});

		final Composite tableContainer = new Composite(parent, SWT.NULL);
		GridLayout layout3 = new GridLayout(1, false);
		layout3.marginWidth = 0;
		layout3.marginHeight = 0;
		tableContainer.setLayout(layout3);
		gridData = new GridData(GridData.FILL_BOTH);
		tableContainer.setLayoutData(gridData);
		
		mStructuresTable = createStructureTable(tableContainer, SWT.NONE);
		mStructuresTable.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				enableButtons();
			}
		});
			    
  	}
	/**
	 * Create the structures Table and TableColumns.
	 * @param parent composite to add table to
	 * @param mode an SWT style to propagate on the table
	 * @return the newly created SWT table
	 */
	private Table createStructureTable(
			final Composite parent,
			final int mode) {
		
		Table table = new Table(parent, mode | SWT.SINGLE | SWT.FULL_SELECTION
				| SWT.BORDER);
		table.setHeaderVisible(true);
		table.setLinesVisible(true);
		
		final GridData gridData = new GridData(GridData.FILL_BOTH);
		table.setLayoutData(gridData);

		AbstractDialog.createTableColumn(table, SWT.LEFT,
				Messages.structure_jaxb_type_label, 120);
		AbstractDialog.createTableColumn(table, SWT.LEFT,
				Messages.structure_jaxb_package_label, 230);
		AbstractDialog.createTableColumn(table, SWT.LEFT,
				Messages.structure_container_label);
	    
	    return table;
	}

    /**
     * Set buttons visibility depending on table status.
     */
    private void enableButtons() {
		if (mStructuresTable.getItemCount() > 0) {
			if (mStructuresTable.getSelectionCount() > 0) {
				mModifyButton.setEnabled(true);
				mDeleteButton.setEnabled(true);
			} else {
				mModifyButton.setEnabled(false);
				mDeleteButton.setEnabled(false);
			}
		} else {
			mModifyButton.setEnabled(false);
			mDeleteButton.setEnabled(false);
		}
    }
	/**
	 * Add button was clicked, present operation dialog.
	 */
	private void handleAdd() {
		CixsStructure structure = new CixsStructure();
		LegacyStructureDialog dlg =
			new LegacyStructureDialog(
					mPluginID, getShell(), mMappingFile, structure);
		if (Window.OK == dlg.open()) {
	        TableItem ti = new TableItem(mStructuresTable, SWT.NONE);
	        ti.setText(structure.getAsStringArray());
		}
		mStructures.add(structure);
		enableButtons();
	}
	
	/**
	 * Modify button was clicked, present operation dialog.
	 */
	private void handleModify() {
		/* Check that at least one structure is selected */
		if (mStructuresTable.getSelectionIndices().length == 0) {
			MessageDialog.openError(
					null,
					Messages.structure_mapping_error_dialog_title,
					Messages.no_structure_selected_msg);
			return;
		}
		for (int i = 0;
				i < mStructuresTable.getSelectionIndices().length; i++) {
			int idx = mStructuresTable.getSelectionIndices()[i];
			TableItem ti = mStructuresTable.getItem(idx);
			/* Lookup this structure in the structures */
			CixsStructure structure = mStructures.get(idx);
			/* Display editing dialog */
			LegacyStructureDialog dlg =
				new LegacyStructureDialog(
						mPluginID, getShell(), mMappingFile, structure);
			if (Window.OK == dlg.open()) {
		        ti.setText(structure.getAsStringArray());
		        mStructuresTable.showItem(ti);
			}
		}
        enableButtons();
	}
	
	/**
	 * Delete button was clicked, confirm and remove structure.
	 */
	private void handleDelete() {
		/* Check that at least one structure is selected */
		if (mStructuresTable.getSelectionIndices().length == 0) {
			MessageDialog.openError(
					null,
					Messages.structure_mapping_error_dialog_title,
					Messages.no_structure_selected_msg);
			return;
		}
		if (MessageDialog.openConfirm(
				null,
				Messages.structure_mapping_error_dialog_title,
				Messages.confirm_structure_delete_msg)) {
			/* Remove this structure from the structures */
			for (int i = 0; i < mStructuresTable.getSelectionIndices().length;
					i++) {
				mStructures.remove(
						mStructuresTable.getSelectionIndices()[i]);
			}
			mStructuresTable.remove(mStructuresTable.getSelectionIndices());
	        enableButtons();
		}
	}


	/**
	 * Creates items in an SWT table from a CixsStructure list of objects.
	 */
	private void loadStructures() {
		
		mStructuresTable.removeAll();
		
		for (CixsStructure structure : mStructures) {
			TableItem ti = new TableItem(mStructuresTable, SWT.NONE);
			ti.setText(structure.getAsStringArray());
		}
	}
	/**
	 * @return the collection of structures
	 */
	public final List < CixsStructure > getStructures() {
		return mStructures;
	}

	/**
	 * @param structures the collection of structures to set
	 */
	public final void setStructures(
			final List < CixsStructure > structures) {
		mStructures = structures;
	}
	

}

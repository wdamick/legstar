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
package com.legstar.eclipse.plugin.cixsgen.dialogs;

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

/**
 * This control displays a table and add/modify/delete buttons for
 * structures editing.
 */
public class StructuresTable extends Composite {

	/** Add new operation. */
	private Button mAddButton;
	
	/** Modify an operation. */
	private Button mModifyButton;

	/** Delete an operation. */
	private Button mDeleteButton;

	/** SWT Table holding operations attributes. */
	private Table mStructuresTable = null;

	/** Dialog box title. */
	private static final String DIALOG_TITLE = "CIXS Structure";
	
	/** Jaxb type column label. */
	private static final String JAXB_TYPE_COL_LABEL = "JAXB Type name";
	
	/** Jaxb package column label. */
	private static final String JAXB_PKG_COL_LABEL = "JAXB Type package name";
	
	/** CICS Container column label. */
	private static final String CICS_CONTAINER_COL_LABEL = "CICS Container";
	
	/** No structures selected error message. */
	private static final String NO_STRUCT_SELECTED_MSG =
		"No structures selected";
	
	/** Confirm structure remove message. */
	private static final String CONFIRM_STRUCT_DELETE_MSG =
		"Confirm selected structures removal?";
	
	/** Add button label. */
	private static final String ADD_LABEL = "Add...";
	
	/** Modify button label. */
	private static final String MODIFY_LABEL = "Modify...";
	
	/** Delete button label. */
	private static final String DELETE_LABEL = "Delete";
	
	/** The collection of structures. */
	private List < CixsStructure > mStructures;
	
	/** The legacy Web Service name descriptor file. */
	private IFile mServiceFile;
	
	/**
	 * Creates a composite control with a table for structures handling.
	 * @param parent the parent composite
	 * @param style an additional style
	 * @param serviceFile the current service descriptor file
	 * @param structures a set if structures to edit
	 */
	public StructuresTable(
			final Composite parent,
			final int style,
			final IFile serviceFile,
			final List < CixsStructure > structures) {
		super(parent, style);
		mServiceFile = serviceFile;
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

		mAddButton = WidgetCreator.createButton(btnContainer, ADD_LABEL);
		mAddButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				handleAdd();
			}
		});
		mAddButton.setEnabled(true); // Add is always available
		
		mModifyButton = WidgetCreator.createButton(btnContainer, MODIFY_LABEL);
		mModifyButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				handleModify();
			}
		});
		mDeleteButton = WidgetCreator.createButton(btnContainer, DELETE_LABEL);
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

		WidgetCreator.createTableColumn(table, SWT.LEFT,
				JAXB_TYPE_COL_LABEL);
		WidgetCreator.createTableColumn(table, SWT.LEFT,
				JAXB_PKG_COL_LABEL);
		WidgetCreator.createTableColumn(table, SWT.LEFT,
				CICS_CONTAINER_COL_LABEL);
	    
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
			new LegacyStructureDialog(getShell(), mServiceFile, structure);
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
					DIALOG_TITLE,
					NO_STRUCT_SELECTED_MSG);
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
				new LegacyStructureDialog(getShell(), mServiceFile, structure);
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
					DIALOG_TITLE,
					NO_STRUCT_SELECTED_MSG);
			return;
		}
		if (MessageDialog.openConfirm(
				null,
				DIALOG_TITLE,
				CONFIRM_STRUCT_DELETE_MSG)) {
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

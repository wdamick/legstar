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

import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;

import com.legstar.eclipse.plugin.cixsgen.Activator;
import com.legstar.eclipse.plugin.cixsgen.model.CixsGenDocument;

/**
 * This dialog is meant to be used as part of a wizard (either new Web Service
 * or editing an existing one. It provides for operation to be added, modified
 * or removed from a Web service.
 *
 * @author Fady Moussallam
 */
public class LegacyWebServiceDialog extends Composite {

	/** SWT Table holding operations attributes. */
	private Table mTable = null;
	
	/** The document model for the Web Service. */
	private CixsGenDocument mCixsGenDoc;

	/** Event type corresponding to an update of the operation dialog. */
	private static final int CHG_EVENT = 1035;
	
	/** Event type corresponding to a request to generate the endpoint. */
	private static final int GEN_EVENT = 1036;
	
	/** Title that should appear on message dialogs.*/
	private static final String DLG_TITLE = "CIXS Web Service generation";
	
	/** Add new operation. */
	private Button mAddButton;
	
	/** Modify an operation. */
	private Button mModifyButton;

	/** Delete an operation. */
	private Button mDeleteButton;

	/** Start the generation process. */
	private Button mGenerateButton;
	
	/** The image on each operation item. */
	private Image mTableImage;
	
	/** The legacy web service description file. */
	private IFile mWsFile;
	
	/** The legacy web service name. */
	private String mWsName;
	
	/** Operation image location. */
	private static final String OPERATION_IMG = "icons/cixsgen-operation.gif";
	
	/** Operations list label. */
	private static final String OPERATIONS_LABEL = "CIXS Operations for ";
	
	/** Add button label. */
	private static final String ADD_LABEL = "Add...";
	
	/** Modify button label. */
	private static final String MODIFY_LABEL = "Modify...";
	
	/** Delete button label. */
	private static final String DELETE_LABEL = "Delete";
	
	/** Generate button label. */
	private static final String GENERATE_LABEL = "Generate";
	
	/** Operation name column label. */
	private static final String OPERATION_COL_LABEL = "CIXS Operation";
	
	/** CICS program name column label. */
	private static final String PROGRAM_COL_LABEL = "CICS Program";
	
	/** Package name of input class name. */
	private static final String IN_PACKAGE_COL_LABEL =
		"Input class package name";
	
	/** Name of input class. */
	private static final String IN_CLASS_COL_LABEL = "Input class name";
	
	/** Package name of output class name. */
	private static final String OUT_PACKAGE_COL_LABEL =
		"Output class package name";
	
	/** Name of output class. */
	private static final String OUT_CLASS_COL_LABEL = "Output class name";
	
	/** No operations selected error message. */
	private static final String NO_OP_SELECTED_MSG = "No operations selected";
	
	/** Confirm operation remove message. */
	private static final String CONFIRM_OP_DELETE_MSG =
		"Confirm selected operations removal?";
	
	/** No operations defined error message. */
	private static final String NO_OPERATIONS_MSG =
		"There must be at least one operation";
	
	/**
	 * Contructor for Legacy Web Service dialog.
	 * @param parent the composite using this dialog
	 * @param style an SWT style to propagate on this composite
	 * @param cixsgenDoc the document model for the legacy Web Service
	 * @param wsFile the legacy web service descriptor file
	 * @param wsName the legacy web service name
	 */
	public LegacyWebServiceDialog(
			final Composite parent,
			final int style,
			final CixsGenDocument cixsgenDoc,
			final IFile wsFile,
			final String wsName) {
		super(parent, style);
		mCixsGenDoc = cixsgenDoc;
		mWsFile = wsFile;
		mWsName = wsName;
		initialize();
	}

	/**
	 * Create widgets on a layout and load document model.
	 */
	private void initialize() {
		mTableImage =
            AbstractUIPlugin.
                imageDescriptorFromPlugin(Activator.PLUGIN_ID,
                		OPERATION_IMG).createImage();
	
		GridLayout gridLayout = new GridLayout(2, false);
		setLayout(gridLayout);
		GridData gridData = new GridData(GridData.FILL_BOTH);
		setLayoutData(gridData);

		createControls(this);
		loadOperations(mCixsGenDoc, mTable);
		enableButtons();
	}
	
	/**
	 * @see java.lang.Object#finalize()
	 */
	protected final void finalize() {
		mTableImage.dispose();
	}

	/**
	 * Create the inner widgets.
	 * @param parent the parent widget	
	 */
	private void createControls(final Composite parent) {

		final Label label = new Label(this, SWT.NONE);
		label.setText(OPERATIONS_LABEL + mWsName);
		GridData gridData = new GridData();
		gridData.horizontalIndent = 5;
		gridData.horizontalSpan = 2;
		label.setLayoutData(gridData);
		
		final Composite btnContainer = new Composite(parent, SWT.NULL);
		GridLayout layout2 = new GridLayout(1, true);
		btnContainer.setLayout(layout2);
		gridData = new GridData();
		gridData.verticalAlignment = GridData.BEGINNING;
		btnContainer.setLayoutData(gridData);

		mAddButton = createButton(btnContainer, ADD_LABEL);
		mAddButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				handleAdd();
			}
		});
		mAddButton.setEnabled(true); // Add is always available
		
		mModifyButton = createButton(btnContainer, MODIFY_LABEL);
		mModifyButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				handleModify();
			}
		});
		mDeleteButton = createButton(btnContainer, DELETE_LABEL);
		mDeleteButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				handleDelete();
			}
		});
		mGenerateButton = createButton(btnContainer, GENERATE_LABEL);
		mGenerateButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				handleGenerate();
			}
		});

		final Composite tableContainer = new Composite(parent, SWT.NULL);
		GridLayout layout3 = new GridLayout(1, true);
		tableContainer.setLayout(layout3);
		gridData = new GridData(GridData.FILL_BOTH);
		tableContainer.setLayoutData(gridData);
		
		
		mTable = createTable(tableContainer, SWT.NONE);
		mTable.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				enableButtons();
			}
		});
			    
  	}
	
	/**
	 * Add a new button on a composite. Is in disabled state initially.
	 * @param parent the parent composite
	 * @param text text to appear on button
	 * @return the newly created button
	 */
	private Button createButton(
			final Composite parent, final String text) {
		Button button = new Button(parent, SWT.PUSH);
		button.setText(text);
		button.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		button.setEnabled(false);
		return button;
	}
	
    /**
     * Set buttons visibility depending on table status.
     */
    private void enableButtons() {
		if (mTable.getItemCount() > 0) {
			mGenerateButton.setEnabled(true);
			if (mTable.getSelectionCount() > 0) {
				mModifyButton.setEnabled(true);
				mDeleteButton.setEnabled(true);
			} else {
				mModifyButton.setEnabled(false);
				mDeleteButton.setEnabled(false);
			}
		} else {
			mGenerateButton.setEnabled(false);
			mModifyButton.setEnabled(false);
			mDeleteButton.setEnabled(false);
		}
    }
    
	/**
	 * Create the operations Table and TableColumns.
	 * @param parent composite to add table to
	 * @param mode an SWT style to propagate on the table
	 * @return the newly created SWT table
	 */
	private Table createTable(
			final Composite parent,
			final int mode) {
		
		Table table = new Table(parent, mode | SWT.SINGLE | SWT.FULL_SELECTION
				| SWT.BORDER);
		table.setHeaderVisible(true);
		table.setLinesVisible(true);
		
		final GridData gridData = new GridData(GridData.FILL_BOTH);
		table.setLayoutData(gridData);

	    createTableColumn(table, SWT.LEFT, OPERATION_COL_LABEL);
	    createTableColumn(table, SWT.LEFT, PROGRAM_COL_LABEL);
	    createTableColumn(table, SWT.LEFT, IN_PACKAGE_COL_LABEL);
	    createTableColumn(table, SWT.LEFT, IN_CLASS_COL_LABEL);
	    createTableColumn(table, SWT.LEFT, OUT_PACKAGE_COL_LABEL);
	    createTableColumn(table, SWT.LEFT, OUT_CLASS_COL_LABEL);
	    
	    return table;
	}
	
	/**
	 * Add a column to an SWT table.
	 * @param table table to add column to
	 * @param style an SWT style for the column
	 * @param title the column header text
	 * @return the newly created column
	 */
	private TableColumn createTableColumn(
			final Table table,
			final int style,
			final String title) {
	    TableColumn tc = new TableColumn(table, style);
	    tc.setText(title);
	    tc.setResizable(true);
	    tc.pack();
	    return tc;
	}
	
	/**
	 * Add button was clicked, present operation dialog.
	 */
	private void handleAdd() {
		LegacyOperationDialog dlg =
			new LegacyOperationDialog(getShell(), mWsFile);
		if (Window.OK == dlg.open()) {
			String[] item = {dlg.getOperationName(),
					dlg.getOperationProgram(),
					dlg.getOperationInputPackage(),
					dlg.getOperationInputType(),
					dlg.getOperationOutputPackage(),
					dlg.getOperationOutputType()};
	        TableItem ti = new TableItem(mTable, SWT.NONE);
	        ti.setText(item);
	        ti.setImage(mTableImage);
			synchronizeModelView();
		}
	}
	
	/**
	 * Modify button was clicked, present operation dialog.
	 */
	private void handleModify() {
		/* Check that at least one operation is selected */
		if (mTable.getSelectionIndices().length == 0) {
			MessageDialog.openError(
					null,
					DLG_TITLE,
					NO_OP_SELECTED_MSG);
			return;
		}
		for (int i = 0; i < mTable.getSelectionIndices().length; i++) {
			TableItem ti = mTable.getItem(mTable.getSelectionIndices()[i]);
			LegacyOperationDialog dlg =
				new LegacyOperationDialog(getShell(), mWsFile);
			dlg.setOperationName(ti.getText(0));
			dlg.setOperationProgram(ti.getText(1));
			dlg.setOperationInputPackage(ti.getText(2));
			dlg.setOperationInputType(ti.getText(3));
			dlg.setOperationOutputPackage(ti.getText(4));
			dlg.setOperationOutputType(ti.getText(5));
			if (Window.OK == dlg.open()) {
				String[] item = {
						dlg.getOperationName(),
						dlg.getOperationProgram(),
						dlg.getOperationInputPackage(),
						dlg.getOperationInputType(),
						dlg.getOperationOutputPackage(),
						dlg.getOperationOutputType()};
		        ti.setText(item);
		        mTable.showItem(ti);
			}
		}
		synchronizeModelView();
	}
	
	/**
	 * Delete button was clicked, confirm and remove operation.
	 */
	private void handleDelete() {
		/* Check that at least one operation is selected */
		if (mTable.getSelectionIndices().length == 0) {
			MessageDialog.openError(
					null,
					DLG_TITLE,
					NO_OP_SELECTED_MSG);
			return;
		}
		if (MessageDialog.openConfirm(
				null,
				DLG_TITLE,
				CONFIRM_OP_DELETE_MSG)) {
			mTable.remove(mTable.getSelectionIndices());
			synchronizeModelView();
		}
	}
	
	/**
	 * Make sure model and view are in sync.
	 */
	private void synchronizeModelView() {
        saveOperations(mCixsGenDoc, mTable);
        enableButtons();
	}
	
	/**
	 * Reauest to genererate is controlled and passed back to controler.
	 */
	private void handleGenerate() {
		/* There must be at least one operation */
		if (mTable.getItemCount() == 0) {
			MessageDialog.openError(
					null,
					DLG_TITLE,
					NO_OPERATIONS_MSG);
			return;
		}
		synchronizeModelView();
		this.notifyListeners(GEN_EVENT, null);
	}
	
	/**
	 * Whenever the model is modified from the outside, this method allows
	 * the control to reset its content.
	 * @param cixsGenDoc the model properties file
	 */
	public final void resetOperations(final CixsGenDocument cixsGenDoc) {
		mCixsGenDoc = cixsGenDoc;
		loadOperations(cixsGenDoc, mTable);
		enableButtons();
	}
	
	/**
	 * Creates items in an SWT table from a properties file where items
	 * are store as indexed properties (using prop.i key form).
	 * @param cixsGenDoc  the model properties file
	 * @param table the SWT table
	 */
	private void loadOperations(
			final CixsGenDocument cixsGenDoc,
			final Table table) {
		
		if (cixsGenDoc == null) {
			return;
		}
		table.removeAll();
		
		int idx = 1;
		boolean opFound = true;
		while (opFound) {
			String sfx = "." + Integer.toString(idx);
			String opprop = CixsGenDocument.OP_NAME + sfx;
			if (cixsGenDoc.getProperty(opprop) != null
					&& cixsGenDoc.getProperty(opprop).length() > 0) {
				TableItem ti = new TableItem(table, SWT.NONE);
				String[] item = {
				 cixsGenDoc.getProperty(opprop),
				 cixsGenDoc.getProperty(CixsGenDocument.OP_PROG + sfx, ""),
				 cixsGenDoc.getProperty(CixsGenDocument.IN_PKG + sfx, ""),
				 cixsGenDoc.getProperty(CixsGenDocument.IN_TYPE + sfx, ""),
				 cixsGenDoc.getProperty(CixsGenDocument.OUT_PKG + sfx, ""),
				 cixsGenDoc.getProperty(CixsGenDocument.OUT_TYPE + sfx, "")
				};
				ti.setText(item);
		        ti.setImage(mTableImage);
				opFound = true;
			} else {
				opFound = false;
			}
			idx += 1; 
		}
		
	}
	
	/**
	 * Serializes an SWT table items as indexed properties in the model 
	 * properties file.
	 * @param cixsGenDoc the model properties file
	 * @param table the SWT table
	 */
	private void saveOperations(
			final CixsGenDocument cixsGenDoc,
			final Table table) {
		if (cixsGenDoc == null) {
			return;
		}
		clearOperations(cixsGenDoc);

		int idx = 1;
		for (int i = 0; i < table.getItems().length; i++) {
			TableItem ti = table.getItem(i);
			String sfx = "." + Integer.toString(idx);
			cixsGenDoc.setProperty(
					CixsGenDocument.OP_NAME + sfx, ti.getText(0));
			cixsGenDoc.setProperty(
					CixsGenDocument.OP_PROG + sfx, ti.getText(1));
			cixsGenDoc.setProperty(
					CixsGenDocument.IN_PKG + sfx, ti.getText(2));
			cixsGenDoc.setProperty(
					CixsGenDocument.IN_TYPE + sfx, ti.getText(3));
			cixsGenDoc.setProperty(
					CixsGenDocument.OUT_PKG + sfx, ti.getText(4));
			cixsGenDoc.setProperty(
					CixsGenDocument.OUT_TYPE + sfx, ti.getText(5));
			idx += 1; 
		}
		
		this.notifyListeners(CHG_EVENT, null);
	}

	/**
	 * Removes all operations from the model properties file.
	 * @param cixsGenDoc the model properties file
	 */
	private void clearOperations(final CixsGenDocument cixsGenDoc) {
		if (cixsGenDoc == null) {
			return;
		}
		int idx = 1;
		boolean opFound = true;
		while (opFound) {
			String sfx = "." + Integer.toString(idx);
			String opprop = CixsGenDocument.OP_NAME + sfx;
			if (cixsGenDoc.getProperty(opprop) != null
					&& cixsGenDoc.getProperty(opprop).length() > 0) {
				cixsGenDoc.remove(opprop);
				cixsGenDoc.remove(CixsGenDocument.OP_PROG + sfx);
				cixsGenDoc.remove(CixsGenDocument.IN_PKG + sfx);
				cixsGenDoc.remove(CixsGenDocument.IN_TYPE + sfx);
				cixsGenDoc.remove(CixsGenDocument.OUT_PKG + sfx);
				cixsGenDoc.remove(CixsGenDocument.OUT_TYPE + sfx);
				opFound = true;
			} else {
				opFound = false;
			}
			idx += 1; 
		}
	}

	/**
	 * @return the cixsGenDoc
	 */
	public final CixsGenDocument getWsgenDoc() {
		return mCixsGenDoc;
	}
}

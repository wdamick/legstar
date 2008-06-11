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
package com.legstar.eclipse.plugin.cixsmap.dialogs;

import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;

import com.legstar.cixs.gen.model.CixsMappingModel;
import com.legstar.cixs.gen.model.CixsModelException;
import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.eclipse.plugin.cixsmap.Activator;

/**
 * This composite is meant to be used as part of a wizard (either new Mapping
 * file or editing an existing one. It provides for operations to be added,
 * modified or removed from a Mapping file.
 *
 * @author Fady Moussallam
 */
public class LegacyMappingComposite extends Composite {

	/** SWT Table holding operations attributes. */
	private Table mOperationsTable = null;
	
	/** The document model for the mapping. */
	private CixsMappingModel mMappingModel;

	/** Title that should appear on message dialogs.*/
	private static final String DLG_TITLE = "CIXS Legacy operations mapping";
	
	/** The legacy mapping file. */
	private IFile mMappingFile;
	
    /** Mapping name name Text. */
    private Text mNameText = null;

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
	
	/** Operation image location. */
	private static final String OPERATION_IMG = "icons/cixsgen-operation.gif";
	
	/** Mapping name label. */
	private static final String MAPPING_NAME_LABEL = "Mapping name:";
	
	/** Add button label. */
	private static final String ADD_LABEL = "Add...";
	
	/** Modify button label. */
	private static final String MODIFY_LABEL = "Modify...";
	
	/** Delete button label. */
	private static final String DELETE_LABEL = "Delete";
	
	/** Generate button label. */
	private static final String GENERATE_LABEL = "Generate";
	
	/** Operation name column label. */
	private static final String OPERATION_COL_LABEL = "Operation";
	
	/** CICS program name column label. */
	private static final String PROGRAM_COL_LABEL = "CICS Program";
	
	/** CICS channel column label. */
	private static final String CHANNEL_COL_LABEL = "CICS Channel";
	
	/** Number of input structures column label. */
	private static final String NB_INPUTS_COL_LABEL = "Input structures";
	
	/** Number of output structures column label. */
	private static final String NB_OUTPUTS_COL_LABEL = "Output structures";
	
	/** No operations selected error message. */
	private static final String NO_OP_SELECTED_MSG = "No operations selected";
	
	/** Confirm operation remove message. */
	private static final String CONFIRM_OP_DELETE_MSG =
		"Confirm selected operations removal?";
	
	/** No operations defined error message. */
	private static final String NO_OPERATIONS_MSG =
		"There must be at least one operation";
	
    /** Event type corresponding to an update on operation dialog composite. */
    public static final int CHG_EVENT = 1035;

    /** Event type corresponding to a request to generate the endpoint. */
    public static final int GEN_EVENT = 1036;
    
    /** All fixed size text boxes will be that large. */
    private static final int TEXT_WIDGETS_WIDTH = 300;

	/**
	 * Constructor for Legacy mapping dialog.
	 * @param parent the composite using this dialog
	 * @param style an SWT style to propagate on this composite
	 * @param mappingModel the document model for the legacy mapping
	 * @param mappingFile the mapping file
	 */
	public LegacyMappingComposite(
			final Composite parent,
			final int style,
			final CixsMappingModel mappingModel,
			final IFile mappingFile) {
		super(parent, style);
		mMappingModel = mappingModel;
		mMappingFile = mappingFile;
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
	
		GridLayout gridLayout = new GridLayout(3, false);
		setLayout(gridLayout);
		GridData gridData = new GridData(GridData.FILL_BOTH);
		setLayoutData(gridData);

		createControls(this);
		loadOperations();
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

		AbstractDialog.createLabel(parent, MAPPING_NAME_LABEL);
		mNameText = AbstractDialog.createText(
		        parent, mMappingModel.getName(), TEXT_WIDGETS_WIDTH);
		mNameText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                mMappingModel.setName(mNameText.getText().trim());
                postProcessChange();
            }
        });
        AbstractDialog.createLabel(parent, "");

		final Composite btnContainer = new Composite(parent, SWT.NULL);
		btnContainer.setLayout(new GridLayout(1, false));
		GridData gridData = new GridData();
		gridData.verticalAlignment = GridData.BEGINNING;
		btnContainer.setLayoutData(gridData);

		mAddButton = AbstractDialog.createButton(btnContainer, ADD_LABEL);
		mAddButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				handleAdd();
			}
		});
		mAddButton.setEnabled(true); // Add is always available
		
		mModifyButton = AbstractDialog.createButton(btnContainer, MODIFY_LABEL);
		mModifyButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				handleModify();
			}
		});
		mDeleteButton = AbstractDialog.createButton(btnContainer, DELETE_LABEL);
		mDeleteButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				handleDelete();
			}
		});
		mGenerateButton = AbstractDialog.createButton(
				btnContainer, GENERATE_LABEL);
		mGenerateButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				handleGenerate();
			}
		});
		
		

		final Composite tableContainer = new Composite(parent, SWT.NULL);
		GridLayout layout3 = new GridLayout(1, true);
		layout3.marginWidth = 0;
		layout3.marginHeight = 0;
		tableContainer.setLayout(layout3);
		gridData = new GridData(GridData.FILL_BOTH);
		gridData.horizontalSpan = 2;
		tableContainer.setLayoutData(gridData);
		
		
		mOperationsTable = createTable(tableContainer, SWT.NONE);
		mOperationsTable.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				enableButtons();
			}
		});
			    
  	}
	
    /**
     * Set buttons visibility depending on table status.
     */
    private void enableButtons() {
		if (mOperationsTable.getItemCount() > 0) {
			mGenerateButton.setEnabled(true);
			if (mOperationsTable.getSelectionCount() > 0) {
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

		AbstractDialog.createTableColumn(table, SWT.LEFT, OPERATION_COL_LABEL);
		AbstractDialog.createTableColumn(table, SWT.LEFT, PROGRAM_COL_LABEL);
		AbstractDialog.createTableColumn(table, SWT.LEFT, CHANNEL_COL_LABEL);
		AbstractDialog.createTableColumn(table, SWT.LEFT, NB_INPUTS_COL_LABEL);
		AbstractDialog.createTableColumn(table, SWT.LEFT, NB_OUTPUTS_COL_LABEL);
	    
	    return table;
	}
	
	/**
	 * Add button was clicked, present operation dialog.
	 */
	private void handleAdd() {
		CixsOperation operation = new CixsOperation();
		LegacyOperationDialog dlg =
			new LegacyOperationDialog(
					Activator.PLUGIN_ID, getShell(), mMappingFile, operation);
		if (Window.OK == dlg.open()) {
	        try {
				mMappingModel.addCixsOperation(operation);
			} catch (CixsModelException e) {
				MessageDialog.openError(
						null,
						DLG_TITLE,
						e.getMessage());
				return;
			}
	        TableItem ti = new TableItem(mOperationsTable, SWT.NONE);
	        ti.setText(operation.getAsStringArray());
	        ti.setImage(mTableImage);
			postProcessChange();
		}
	}
	
	/**
	 * Modify button was clicked, present operation dialog.
	 */
	private void handleModify() {
		/* Check that at least one operation is selected */
		if (mOperationsTable.getSelectionIndices().length == 0) {
			MessageDialog.openError(
					null,
					DLG_TITLE,
					NO_OP_SELECTED_MSG);
			return;
		}
		for (int i = 0;
				i < mOperationsTable.getSelectionIndices().length; i++) {
			int idx = mOperationsTable.getSelectionIndices()[i];
			TableItem ti = mOperationsTable.getItem(idx);
			/* Lookup this operation in the service */
			CixsOperation operation =
				mMappingModel.getCixsOperations().get(idx);
			/* Display editing dialog */
			LegacyOperationDialog dlg =
				new LegacyOperationDialog(
						Activator.PLUGIN_ID, getShell(), mMappingFile,
						operation);
			if (Window.OK == dlg.open()) {
		        ti.setText(operation.getAsStringArray());
		        mOperationsTable.showItem(ti);
				postProcessChange();
			}
		}
	}
	
	/**
	 * Delete button was clicked, confirm and remove operation.
	 */
	private void handleDelete() {
		/* Check that at least one operation is selected */
		if (mOperationsTable.getSelectionIndices().length == 0) {
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
			/* Remove this operation from the service */
			for (int i = 0; i < mOperationsTable.getSelectionIndices().length;
					i++) {
				mMappingModel.getCixsOperations().remove(
						mOperationsTable.getSelectionIndices()[i]);
			}
			mOperationsTable.remove(mOperationsTable.getSelectionIndices());
			postProcessChange();
		}
	}
	
	/**
	 * Reauest to genererate is controlled and passed back to controler.
	 */
	private void handleGenerate() {
		/* There must be at least one operation */
		if (mOperationsTable.getItemCount() == 0) {
			MessageDialog.openError(
					null,
					DLG_TITLE,
					NO_OPERATIONS_MSG);
			return;
		}
		/* Notify listeners early because we need the generation dialog
		 * to work on a committed mapping file. */
        this.notifyListeners(GEN_EVENT, null);
        GenerateDialog dlg =
            new GenerateDialog(
                    Activator.PLUGIN_ID, getShell(), mMappingFile);
        dlg.open();
        enableButtons();
	}
	
	/**
	 * Perform actions following a change in the UI.
	 * See if buttons should be enabled and notify listeners that
	 * something changed.
	 */
	private void postProcessChange() {
        enableButtons();
		this.notifyListeners(CHG_EVENT, null);
	}
	
	/**
	 * Whenever the model is modified from the outside, this method allows
	 * the control to reset its content.
	 * @param service the service model
	 */
	public final void resetOperations(final CixsMappingModel service) {
		mMappingModel = service;
		loadOperations();
		enableButtons();
	}
	
	/**
	 * Creates items in an SWT table from a CixsOperation object.
	 */
	private void loadOperations() {
		
		mOperationsTable.removeAll();
		
		for (CixsOperation operation : mMappingModel.getCixsOperations()) {
			TableItem ti = new TableItem(mOperationsTable, SWT.NONE);
			ti.setText(operation.getAsStringArray());
	        ti.setImage(mTableImage);
		}
	}
	
	/**
	 * @return the mapping model
	 */
	public final CixsMappingModel getMappingModel() {
		return mMappingModel;
	}
}

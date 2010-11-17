/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
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
import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.eclipse.plugin.cixsmap.Activator;
import com.legstar.eclipse.plugin.cixsmap.Messages;
import com.legstar.eclipse.plugin.common.dialogs.AbstractDialog;

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

        AbstractDialog.createLabel(parent,
                Messages.operations_mapping_name_label + ':');
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
        mGenerateButton = AbstractDialog.createButton(
                btnContainer, Messages.generate_button_label);
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

        AbstractDialog.createTableColumn(table, SWT.LEFT,
                Messages.operation_name_label);
        AbstractDialog.createTableColumn(table, SWT.LEFT,
                Messages.operation_program_label);
        AbstractDialog.createTableColumn(table, SWT.LEFT,
                Messages.operation_channel_label);
        AbstractDialog.createTableColumn(table, SWT.RIGHT,
                Messages.operation_input_structures_nbr_label);
        AbstractDialog.createTableColumn(table, SWT.RIGHT,
                Messages.operation_output_structures_nbr_label);

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
            mMappingModel.addCixsOperation(operation);
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
                    Messages.operation_mapping_error_dialog_title,
                    Messages.no_operations_selected_msg);
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
                    Messages.operation_mapping_error_dialog_title,
                    Messages.no_operations_selected_msg);
            return;
        }
        if (MessageDialog.openConfirm(
                null,
                Messages.operation_mapping_error_dialog_title,
                Messages.confirm_operation_delete_msg)) {
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
                    Messages.operation_mapping_error_dialog_title,
                    Messages.no_operations_msg);
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
    public void resetOperations(final CixsMappingModel service) {
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
    public CixsMappingModel getMappingModel() {
        return mMappingModel;
    }
}

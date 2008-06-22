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

import java.util.List;

import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.layout.GridData;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.gen.model.CixsStructure;
import com.legstar.eclipse.plugin.cixsmap.Activator;
import com.legstar.eclipse.plugin.cixsmap.Messages;
import com.legstar.eclipse.plugin.common.dialogs.AbstractDialog;

/**
 * Dialog used to capture a legacy Web service operation attributes.
 *
 * @author Fady Moussallam
 * 
 */
public class LegacyOperationDialog extends AbstractDialog {

	/** Maximum number of characters in a host program name. */
	private static final int HOSTPGM_LEN = 8;
	
	/** The operation being edited. */
	private CixsOperation mOperation;

	/** Dialog widgets. */
	/** Operation Text. */
	private Text mOperationNameText = null;
	/** Program Text. */
	private Text mCicsProgramNameText = null;
	/** CICS Channel Text. */
	private Text mCicsChannelText = null;
	/** Input structures control. */
	private StructuresTableComposite mInputStructuresTable = null;
	/** Output structures control. */
	private StructuresTableComposite mOutputStructuresTable = null;
	
	/** Indicates if host program name should be automatically derived from
     * operation name. */
	private boolean pgnameAuto = true;
	/** Determines if host program name is automatically generated or set by
	 * the user. */
	private boolean pgnameAutoSet = false;
	
	/** The legacy mapping file. */
	private IFile mMappingFile;
	
	/**
	 * Constructor for operation dialog.
	 * @param pluginID the current plugin ID
	 * @param parentShell the parent shell
	 * @param mappingFile the mapping file
	 * @param operation the operation being edited
	 */
	public LegacyOperationDialog(
			final String pluginID,
			final Shell parentShell,
			final IFile mappingFile,
			final CixsOperation operation) {
		super(parentShell, pluginID);
		mMappingFile = mappingFile;
		mOperation = operation;
	}
	
	/** {@inheritDoc}	 */
	protected final Control createDialogArea(final Composite parent) {
		Composite composite = (Composite) super.createDialogArea(parent);
		initialize(composite);
		return composite;
	}

	/**
	 * Create dialog widgets.
	 * @param parent the parent composite
	 */
	private void initialize(final Composite parent) {

		Composite area = new Composite(parent, SWT.NULL);
		GridLayout gridLayout = new GridLayout(2, false);
		area.setLayout(gridLayout);

		createLabel(area, Messages.operation_name_label + ':');
		mOperationNameText = createText(area,
				mOperation.getName(), -1);
		mOperationNameText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				/* Automatically populate host name when operation changes */
				String operationName = mOperationNameText.getText().trim();
				if (pgnameAuto && operationName.length() > 0) {
					int ol =
						(operationName.length()  > HOSTPGM_LEN)
						? HOSTPGM_LEN : operationName.length();
					pgnameAutoSet = true;
					mCicsProgramNameText.setText(
							operationName.
							substring(0, ol).toUpperCase());
				}
			}
		});
		
		createLabel(area, Messages.operation_program_label + ':');
		mCicsProgramNameText = createText(area,
				mOperation.getCicsProgramName(), -1);
		mCicsProgramNameText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				/* User typed in something so stop generating automatically */
				if (pgnameAutoSet) {
					pgnameAutoSet = false;
				} else {
					pgnameAuto = false;
				}
			}
		});
		
		createLabel(area, Messages.operation_channel_label + ':');
		mCicsChannelText = createText(area,
				mOperation.getCicsChannel(), -1);
		mCicsChannelText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
			}
		});
		
		createLabel(area, Messages.operation_input_structures_label + ':');
		mInputStructuresTable = addStructuresTable(
				area, mOperation.getInput());

		createLabel(area, Messages.operation_output_structures_label + ':');
		mOutputStructuresTable = addStructuresTable(
				area, mOperation.getOutput());
	}
	
	/**
	 * Structures tables are a composite control with a table and buttons to
	 * add, modify and delete rows.
	 * @param area parent composite
	 * @param structures teh set of structures
	 * @return the new structures table widget
	 */
	private StructuresTableComposite addStructuresTable(
			final Composite area,
			final List < CixsStructure > structures) {
		StructuresTableComposite structuresTable =
			new StructuresTableComposite(
					getPluginID(), area, SWT.NONE, mMappingFile, structures);
		GridData gdRight = new GridData(GridData.FILL_HORIZONTAL);
		structuresTable.setLayoutData(gdRight);
		return structuresTable;
	}

	/** {@inheritDoc}	 */
	protected final void configureShell(final Shell newShell) {
		super.configureShell(newShell);
		newShell.setText(Messages.operation_mapping_dialog_title);
		ImageDescriptor image =
            AbstractUIPlugin.
                imageDescriptorFromPlugin(
                		Activator.PLUGIN_ID,
                		Messages.operations_mapping_icon);
		newShell.setImage(image.createImage());
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#okPressed()
	 */
	 /**  {@inheritDoc} */
	protected final void okPressed() {
		if (!validDialogData()) {
			return;
		}
		setReturnCode(OK);
		mOperation.setName(mOperationNameText.getText().trim());
		mOperation.setCicsProgramName(mCicsProgramNameText.getText().trim());
		mOperation.setCicsChannel(mCicsChannelText.getText().trim());
		mOperation.setInput(mInputStructuresTable.getStructures());
		mOperation.setOutput(mOutputStructuresTable.getStructures());
		close();
	}

	/**
	 * Ensures that all text fields are set.
	 * @return true if data is valid
	 */
	private boolean validDialogData() {
		
		/* We must have an operation name */
		String operationName = mOperationNameText.getText().trim();
		if (operationName.length() == 0) {
			errorDialog(Messages.operation_mapping_error_dialog_title,
					Messages.no_operation_name_msg);
			return false;
		}
		
		/* We must have an program name */
		String cicsProgramName = mCicsProgramNameText.getText().trim();
		if (cicsProgramName.length() == 0) {
			errorDialog(Messages.operation_mapping_error_dialog_title,
			        Messages.no_program_msg);
			return false;
		}
		
		if (!validStructures(mOperation.getInput())) {
			return false;
		}
		if (!validStructures(mOperation.getOutput())) {
			return false;
		}


		return true;
	}
		
	/**
	 * If CICS Channel is specified then CICS Containers must be specified
	 * and vice-versa.
	 * @param structures input or output structures collections
	 * @return false if validation fails
	 */
	private boolean validStructures(
			final List < CixsStructure > structures) {
		/*  */
		String channel = mCicsChannelText.getText().trim();
		for (CixsStructure structure : structures) {
			if (structure.getCicsContainer() != null
					&& structure.getCicsContainer().length() > 0) {
				if (channel.length() == 0) {
					errorDialog(Messages.operation_mapping_error_dialog_title,
							Messages.container_without_channel_msg);
					return false;
				}
			} else {
				if (channel.length() > 0) {
					errorDialog(Messages.operation_mapping_error_dialog_title,
							Messages.channel_without_container_msg);
					return false;
				}
			}
		}
		return true;
	}
	
	/**
	 * @return operation
	 */
	public final CixsOperation getOperation() {
		return mOperation;
	}

	/**
	 * @param operation operation
	 */
	public final void setOperation(
			final CixsOperation operation) {
		this.mOperation = operation;
	}

}

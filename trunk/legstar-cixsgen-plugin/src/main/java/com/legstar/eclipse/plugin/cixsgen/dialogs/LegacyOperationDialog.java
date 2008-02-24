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

import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Text;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.layout.GridData;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.gen.model.CixsStructure;

/**
 * Dialog used to capture a legacy Web service operation attributes.
 *
 * @author Fady Moussallam
 * 
 */
public class LegacyOperationDialog extends Dialog {

	/** Maximum number of characters in a host program name. */
	private static final int HOSTPGM_LEN = 8;
	
	/** Used in error messages. */
	private String mPluginID;
	
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
	private StructuresTable mInputStructuresTable = null;
	/** Output structures control. */
	private StructuresTable mOutputStructuresTable = null;
	
	/** Indicates if host program name should be automatically derived from
     * operation name. */
	private boolean pgnameAuto = true;
	/** Determines if host program name is automatically generated or set by
	 * the user. */
	private boolean pgnameAutoSet = false;
	
	/** Dialog box title. */
	private static final String DIALOG_TITLE = "CIXS Operation";
	
	/** Operation name label. */
	private static final String OP_NAME_LABEL = "Operation name:";
	
	/** CICS program name label. */
	private static final String PROGRAM_LABEL = "CICS Program:";
	
	/** CICS channel name label. */
	private static final String CHANNEL_LABEL = "CICS Channel:";
	
	/** Input structures label. */
	private static final String INPUT_LABEL = "Input:";
	
	/** Output structures label. */
	private static final String OUTPUT_LABEL = "Output:";
	
	/** The legacy Web Service name descriptor file. */
	private IFile mServiceFile;
	
	/**
	 * Constructor for operation dialog.
	 * @param pluginID the current plugin ID
	 * @param parentShell the parent shell
	 * @param serviceFile the web service descriptor file
	 * @param operation the operation being edited
	 */
	public LegacyOperationDialog(
			final String pluginID,
			final Shell parentShell,
			final IFile serviceFile,
			final CixsOperation operation) {
		super(parentShell);
		mPluginID = pluginID;
		mServiceFile = serviceFile;
		mOperation = operation;
	}
	
	/** {@inheritDoc}	 */
	protected final Control createDialogArea(final Composite parent) {
		Composite composite = (Composite) super.createDialogArea(parent);
		try {
			initialize(composite);
		} catch (CoreException e) {
			return null;
		}
		return composite;
	}

	/**
	 * Create dialog widgets.
	 * @param parent the parent composite
	 * @throws CoreException if creation fails
	 */
	private void initialize(final Composite parent) throws CoreException {

		Composite area = new Composite(parent, SWT.NULL);
		GridLayout gridLayout = new GridLayout(2, false);
		area.setLayout(gridLayout);

		WidgetCreator.createLabel(area, OP_NAME_LABEL);
		mOperationNameText = WidgetCreator.createText(area,
				mOperation.getName());
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
		
		WidgetCreator.createLabel(area, PROGRAM_LABEL);
		mCicsProgramNameText = WidgetCreator.createText(area,
				mOperation.getCicsProgramName());
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
		
		WidgetCreator.createLabel(area, CHANNEL_LABEL);
		mCicsChannelText = WidgetCreator.createText(area,
				mOperation.getCicsChannel());
		mCicsChannelText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
			}
		});
		
		WidgetCreator.createLabel(area, INPUT_LABEL);
		mInputStructuresTable = addStructuresTable(
				area, mOperation.getInput());

		WidgetCreator.createLabel(area, OUTPUT_LABEL);
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
	private StructuresTable addStructuresTable(
			final Composite area,
			final List < CixsStructure > structures) {
		StructuresTable structuresTable =
			new StructuresTable(
					mPluginID, area, SWT.NONE, mServiceFile, structures);
		GridData gdRight = new GridData(GridData.FILL_HORIZONTAL);
		structuresTable.setLayoutData(gdRight);
		return structuresTable;
	}

	/** {@inheritDoc}	 */
	protected final void configureShell(final Shell newShell) {
		super.configureShell(newShell);
		newShell.setText(DIALOG_TITLE);
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
			errorDialog(getShell(), "You must provide an operation name");
			return false;
		}
		
		/* We must have an program name */
		String cicsProgramName = mCicsProgramNameText.getText().trim();
		if (cicsProgramName.length() == 0) {
			errorDialog(getShell(), "You must provide an program name");
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
					errorDialog(getShell(), "Structures cannot specify a CICS"
							+ " Container if operation does not specify a CICS"
							+ " Channel");
					return false;
				}
			} else {
				if (channel.length() > 0) {
					errorDialog(getShell(), "All structures must specify a CICS"
							+ " Container when operation specifies a CICS"
							+ " Channel");
					return false;
				}
			}
		}
		return true;
	}
	
    /**
     * Pops an error message.
     * @param shell parent shell
     * @param message text
     */
    private void errorDialog(final Shell shell, final String message) { 
	    IStatus status = new Status(
	    		IStatus.ERROR, mPluginID,
	    		IStatus.ERROR, message, null);         
	    ErrorDialog.openError(shell, "Operation Error", null, status); 
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

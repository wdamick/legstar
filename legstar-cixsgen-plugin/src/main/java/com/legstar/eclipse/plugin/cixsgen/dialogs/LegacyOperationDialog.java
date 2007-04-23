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

import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.layout.GridData;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.core.resources.IFile;

import java.util.ArrayList;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import java.util.Map;
import java.util.HashMap;

import com.legstar.eclipse.plugin.cixsgen.Activator;
import com.legstar.eclipse.plugin.common.LegstarReport;


/**
 * Dialog used to capture a legacy Web service operation aattributes.
 *
 * @author Fady Moussallam
 * 
 */
public class LegacyOperationDialog extends Dialog {

	/** Last segment of coxb binding classes package. */
	private static final String BIND_FRAG = "bind";
	
	/** Coxb binding classes prefix. */
	private static final String BIND_SUFFIX = "Binding";
	
	/** Maximum number of characters in a host program name. */
	private static final int HOSTPGM_LEN = 8;
	
	/** Map package names to their jdt fragment. */
	private Map < String, IPackageFragment > mPkgMap =
		new HashMap < String, IPackageFragment >();
	
	/** The operation name. */
	private String mOperationName = "";
	/** The associated host program name. */
	private String mOperationProgram = "";
	/** The package for input class type. */
	private String mOperationInputPackage = "";
	/** The input class type. */
	private String mOperationInputType = "";
	/** The package for output class type. */
	private String mOperationOutputPackage = "";
	/** The output class type. */
	private String mOperationOutputType = "";

	/** Dialog widgets. */
	/** Operation Text. */
	private Text mOperationNameText = null;
	/** Program Text. */
	private Text mOperationProgramText = null;
	/** Input package combo. */
	private Combo mOperationInputPackageCombo = null;
	/** Input type combo. */
	private Combo mOperationInputTypeCombo = null;
	/** Output package combo. */
	private Combo mOperationOutputPackageCombo = null;
	/** Output type combo. */
	private Combo mOperationOutputTypeCombo = null;
	
	/** Indicates if host program name should be automatically derived from
     * operation name. */
	private boolean pgnameAuto = true;
	/** Determines if host program name is automatically generated or set by
	 * the user. */
	private boolean pgnameAutoSet = false;
	
	/** The legacy web service description file. */
	private IFile mWsFile;

	/** Dialog box title. */
	private static final String DIALOG_TITLE = "CIXS Operation";
	
	/** Operation name label. */
	private static final String OP_NAME_LABEL = "Operation name:";
	
	/** CICS program name label. */
	private static final String PROGRAM_LABEL = "CICS Program:";
	
	/** Package name of input class name. */
	private static final String IN_PACKAGE_COL_LABEL =
		"Input class package name:";
	
	/** Name of input class. */
	private static final String IN_CLASS_COL_LABEL = "Input class name:";
	
	/** Package name of output class name. */
	private static final String OUT_PACKAGE_COL_LABEL =
		"Output class package name:";
	
	/** Name of output class. */
	private static final String OUT_CLASS_COL_LABEL = "Output class name:";
	
	
	/**
	 * Constructor for operation dialog.
	 * @param parentShell the parent shell
	 * @param wsFile the legacy web service descriptor file
	 */
	protected LegacyOperationDialog(
			final Shell parentShell,
			final IFile wsFile) {
		super(parentShell);
		mWsFile = wsFile;
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
		gridLayout.numColumns = 2;
		area.setLayout(gridLayout);
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		area.setLayoutData(gd);

		addLabel(area, OP_NAME_LABEL);
		mOperationNameText = addText(area, mOperationName);
		mOperationNameText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				/* Automatically populate host name when operation changes */
				if (pgnameAuto
						&& mOperationNameText.getText().length() > 0) {
					int ol =
						(mOperationNameText.getText().length()  > HOSTPGM_LEN)
						? HOSTPGM_LEN : mOperationNameText.getText().length();
					pgnameAutoSet = true;
					mOperationProgramText.setText(
							mOperationNameText.getText().
							substring(0, ol).toUpperCase());
				}
			}
		});
		
		addLabel(area, PROGRAM_LABEL);
		mOperationProgramText = addText(area, mOperationProgram);
		mOperationProgramText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				/* User typed in something so stop generating automatically */
				if (pgnameAutoSet) {
					pgnameAutoSet = false;
				} else {
					pgnameAuto = false;
				}
			}
		});
		
		initInputPackageCombo(area);
		initInputTypeCombo(area);
		initOutputPackageCombo(area);
		initOutputTypeCombo(area);
		
		try {
			createPackageList();
			loadPackageCombos();
		} catch (JavaModelException e) {
			LegstarReport.throwCoreException(e, Activator.PLUGIN_ID);
		}
	}
	
	/**
	 * Create the input package combo.
	 * @param area the parent composite
	 */
	private void initInputPackageCombo(final Composite area) {
		addLabel(area, IN_PACKAGE_COL_LABEL);
		mOperationInputPackageCombo = addCombo(area);
		mOperationInputPackageCombo.addSelectionListener(
				new SelectionListener() {
			public void widgetDefaultSelected(final SelectionEvent e) {
				packageListChanged(mOperationInputPackageCombo,
						mOperationInputTypeCombo,
						mOperationInputType);
			}
			public void widgetSelected(final SelectionEvent e) {
				packageListChanged(mOperationInputPackageCombo,
						mOperationInputTypeCombo,
						mOperationInputType);
			}
		});
	}
	
	/**
	 * Create the input type combo.
	 * @param area the parent composite
	 */
	private void initInputTypeCombo(final Composite area) {
		addLabel(area, IN_CLASS_COL_LABEL);
		mOperationInputTypeCombo = addCombo(area);
		mOperationInputTypeCombo.addSelectionListener(new SelectionListener() {
			public void widgetDefaultSelected(final SelectionEvent e) {
				inputTypeListChanged();
			}
			public void widgetSelected(final SelectionEvent e) {
				inputTypeListChanged();
			}
		});
	}
	
	/**
	 * Create the output package combo.
	 * @param area the parent composite
	 */
	private void initOutputPackageCombo(final Composite area) {
		addLabel(area, OUT_PACKAGE_COL_LABEL);
		mOperationOutputPackageCombo = addCombo(area);
		mOperationOutputPackageCombo.addSelectionListener(
				new SelectionListener() {
			public void widgetDefaultSelected(final SelectionEvent e) {
				packageListChanged(mOperationOutputPackageCombo,
						mOperationOutputTypeCombo,
						mOperationOutputType);
			}
			public void widgetSelected(final SelectionEvent e) {
				packageListChanged(mOperationOutputPackageCombo,
						mOperationOutputTypeCombo,
						mOperationOutputType);
			}
		});
	}

	/**
	 * Create the output type combo.
	 * @param area the parent composite
	 */
	private void initOutputTypeCombo(final Composite area) {
		addLabel(area, OUT_CLASS_COL_LABEL);
		mOperationOutputTypeCombo = addCombo(area);
		mOperationOutputTypeCombo.addSelectionListener(new SelectionListener() {
			public void widgetDefaultSelected(final SelectionEvent e) {
				outputTypeListChanged();
			}
			public void widgetSelected(final SelectionEvent e) {
				outputTypeListChanged();
			}
		});
	}
	
	/**
	 * Create a label widget.
	 * @param area parent composite
	 * @param text label text
	 * @return the new label
	 */
	private static Label addLabel(final Composite area, final String text) {
		Label label = new Label(area, SWT.NONE);
		label.setText(text);
		return label;
	}

	/**
	 * Create a Text widget.
	 * @param area parent composite
	 * @param initText the initial content
	 * @return the new text widget
	 */
	private static Text addText(final Composite area, final String initText) {
		Text text = new Text(area, SWT.BORDER);
		text.setLayoutData(
				new GridData(GridData.FILL_HORIZONTAL));
		text.setText(initText);
		return text;
	}

	/**
	 * Create a Combo widget.
	 * @param area parent composite
	 * @return the new combo widget
	 */
	private static Combo addCombo(final Composite area) {
		Combo combo = new Combo(area, SWT.READ_ONLY | SWT.SINGLE
				| SWT.BORDER | SWT.V_SCROLL);
		combo.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		return combo;
	}
	
	/**
	 * Fill the package lists in combo boxes. Strip the final .bind since
	 * we want to show JAXB packages only.
	 */
	private void loadPackageCombos() {
		for (int i = 0; i < mPkgMap.size(); i++) {
			String bindPkg = (String) mPkgMap.keySet().toArray()[i];
			String jaxbPkg = bindPkg.substring(
					0, bindPkg.indexOf("."  + BIND_FRAG, 0));
			mOperationInputPackageCombo.add(jaxbPkg);
			mOperationOutputPackageCombo.add(jaxbPkg);
		}
		/* Make sure an item shows up in the text box */
		if (mPkgMap.size() > 0) {
			int i;
			i = mOperationOutputPackageCombo.indexOf(mOperationOutputPackage);
			if (i > 0) {
				mOperationOutputPackageCombo.select(i);
			} else {
				mOperationOutputPackageCombo.select(0);
			}
			i = mOperationInputPackageCombo.indexOf(mOperationInputPackage);
			if (i > 0) {
				mOperationInputPackageCombo.select(i);
			} else {
				mOperationInputPackageCombo.select(0);
			}
			packageListChanged(mOperationInputPackageCombo,
					mOperationInputTypeCombo,
					mOperationInputType);
			packageListChanged(mOperationOutputPackageCombo,
					mOperationOutputTypeCombo,
					mOperationOutputType);
		}
	}

	/** {@inheritDoc}	 */
	protected final void configureShell(final Shell newShell) {
		super.configureShell(newShell);
		newShell.setText(DIALOG_TITLE);
	}
	
	/**
	 * Populate the types list based on latest package selection.
	 * @param pkgCombo the input or output package combo box
	 * @param typeCombo the input or output type combo box
	 * @param type the current input or output type
	 */
	private void packageListChanged(
			final Combo pkgCombo,
			final Combo typeCombo,
			final String type) {
		IPackageFragment pkgf = mPkgMap.get(
				pkgCombo.getText() + "."  + BIND_FRAG);
		if (pkgf != null) {
			java.util.List < String > typesList = classList(pkgf);
			typeCombo.removeAll();
			for (int i = 0; i < typesList.size(); i++) {
				/* Strip the binding suffix to show JAXB classes names */
				String bindType = typesList.get(i);
				String jaxbType = bindType.substring(
						0, bindType.indexOf(BIND_SUFFIX, 0));
				typeCombo.add(jaxbType);
			}
			if (typesList.size() > 0) {
				int i = typeCombo.indexOf(type);
				if (i > 0) {
					typeCombo.select(i);
				} else {
					typeCombo.select(0);
				}
			}
		}
	}
	
	
	/**
	 * User selected an input type.
	 */
	private void inputTypeListChanged() {
		mOperationInputType = mOperationInputTypeCombo.getText();
	}
	
	/**
	 * User selected an output type.
	 */
	private void outputTypeListChanged() {
		mOperationOutputType = mOperationOutputTypeCombo.getText();
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
		mOperationName = mOperationNameText.getText();
		mOperationProgram = mOperationProgramText.getText();
		mOperationInputPackage = mOperationInputPackageCombo.getText();
		mOperationInputType = mOperationInputTypeCombo.getText();
		mOperationOutputPackage = mOperationOutputPackageCombo.getText();
		mOperationOutputType = mOperationOutputTypeCombo.getText();
		close();
	}

	/**
	 * Examines packages in a given project and adds segments which contain 
	 * the .bind suffix to the package list table. ".bind" suffix identify
	 * segments that correspond to COXB binding packages.
	 * @throws JavaModelException if packages cannot be retrieved
	 */
	private void createPackageList() throws JavaModelException {
		mPkgMap.clear();
		IJavaProject jproject = JavaCore.create(mWsFile.getProject());
		IPackageFragmentRoot[] pkgRoots =
			jproject.getPackageFragmentRoots();
		for (int j = 0; j < pkgRoots.length; j++) {
			IPackageFragmentRoot pkgRoot = pkgRoots[j];
			if (pkgRoot.getKind() ==  IPackageFragmentRoot.K_SOURCE) {
				for (int k = 0; k < pkgRoot.getChildren().length; k++) {
					IJavaElement el = pkgRoot.getChildren()[k];
					if (el.getPath().lastSegment().
							compareTo(BIND_FRAG) == 0) {
						mPkgMap.put(el.getElementName(),
								(IPackageFragment) el);
					}
				}
			}
		}
	}
	
	/**
	 * Given a package fragment, this method gets all the Java compilation
	 *  units.
	 * @param pkg the package fragment
	 * @return a list of Java comilation units names within the package
	 */
	private java.util.List < String > classList(
			final IPackageFragment pkg) {
		java.util.List < String > classes = new ArrayList < String >();
		try {
			ICompilationUnit[] compilationUnits = pkg.getCompilationUnits();
	        for (int k = 0; k < compilationUnits.length; k++) {
	           ICompilationUnit unit = compilationUnits[k];
	           if (unit.isStructureKnown()) {
	        	   IType[] types = unit.getTypes();
	        	   for (int l = 0; l < types.length; l++) {
	        		   classes.add(types[l].getElementName());
	        	   }
	           }
	        }
		} catch (JavaModelException e) {
			return null;
		}
		return classes;
	}

	/**
	 * Ensures that all text fields are set.
	 * @return true if data is valid
	 */
	private boolean validDialogData() {
		
		/* We must have an operation name */
		String operationName = mOperationNameText.getText();
		if (operationName.length() == 0) {
			errorDialog(getShell(), "You must provide an operation name");
			return false;
		}
		
		/* We must have an program name */
		String programName = mOperationProgramText.getText();
		if (programName.length() == 0) {
			errorDialog(getShell(), "You must provide an program name");
			return false;
		}
		
		return true;
	}
	
    /**
     * Pops an error message.
     * @param shell parent shell
     * @param message text
     */
    private static void errorDialog(final Shell shell, final String message) { 
	    IStatus status = new Status(
	    		IStatus.ERROR, Activator.PLUGIN_ID,
	    		IStatus.ERROR, message, null);         
	    ErrorDialog.openError(shell, "Operation Error", null, status); 
    } 

	/**
	 * @return operation name
	 */
	public final String getOperationName() {
		return mOperationName;
	}

	/**
	 * @return host program name
	 */
	public final String getOperationProgram() {
		return mOperationProgram;
	}
	
	/**
	 * @return input package name
	 */
	public final String getOperationInputPackage() {
		return mOperationInputPackage;
	}

	/**
	 * @return input type name
	 */
	public final String getOperationInputType() {
		return mOperationInputType;
	}

	/**
	 * @return output class name
	 */
	public final String getOperationOutputType() {
		return mOperationOutputType;
	}

	/**
	 * @return output package name
	 */
	public final String getOperationOutputPackage() {
		return mOperationOutputPackage;
	}

	/**
	 * @param operationName operation name
	 */
	public final void setOperationName(
			final String operationName) {
		this.mOperationName = operationName;
	}

	/**
	 * @param operationProgram host program name
	 */
	public final void setOperationProgram(
			final String operationProgram) {
		if (operationProgram.length() > 0)  {
			pgnameAuto = false;
		}
		this.mOperationProgram = operationProgram;
	}

	/**
	 * @param operationInputType input class type name
	 */
	public final void setOperationInputType(
			final String operationInputType) {
		this.mOperationInputType = operationInputType;
	}

	/**
	 * @param operationInputPackage input package name
	 */
	public final void setOperationInputPackage(
			final String operationInputPackage) {
		this.mOperationInputPackage = operationInputPackage;
	}

	/**
	 * @param operationOutputType output class type name
	 */
	public final void setOperationOutputType(
			final String operationOutputType) {
		this.mOperationOutputType = operationOutputType;
	}

	/**
	 * @param operationOutputPackage output package name
	 */
	public final void setOperationOutputPackage(
			final String operationOutputPackage) {
		mOperationOutputPackage = operationOutputPackage;
	}


}

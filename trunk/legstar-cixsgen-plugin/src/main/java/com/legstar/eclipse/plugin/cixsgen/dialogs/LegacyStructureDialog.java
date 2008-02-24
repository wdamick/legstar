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
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.jface.dialogs.ErrorDialog;
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

import java.util.Collections;
import java.util.Map;
import java.util.HashMap;

import com.legstar.cixs.gen.model.CixsStructure;
import com.legstar.eclipse.plugin.common.LegstarReport;


/**
 * Dialog used to capture a legacy Web service operation attributes.
 *
 * @author Fady Moussallam
 * 
 */
public class LegacyStructureDialog extends Dialog {

	/** Used in error messages. */
	private String mPluginID;

	/** Last segment of coxb binding classes package. */
	private static final String BIND_FRAG = "bind";
	
	/** Coxb binding classes prefix. */
	private static final String BIND_SUFFIX = "Binding";
	
	/** Map package names to their jdt fragment. */
	private Map < String, IPackageFragment > mPkgMap =
		new HashMap < String, IPackageFragment >();
	
	/** The structure element being edited. */
	private CixsStructure mStructure;

	/** Dialog widgets. */
	/** Jaxb package combo. */
	private Combo mJaxbPackageCombo = null;
	/** Jaxb type list. */
	private List mJaxbTypeList = null;
	/** CICS Container Text. */
	private Text mCicsContainerText = null;
	
	
	/** The legacy web service description file. */
	private IFile mServiceFile;

	/** Dialog box title. */
	private static final String DIALOG_TITLE = "CIXS Structure";
	
	/** CICS container name label. */
	private static final String CONTAINER_LABEL = "CICS Container:";
	
	/** Package name of jaxb type. */
	private static final String PACKAGE_COL_LABEL =
		"JAXB Type package name:";
	
	/** Name of JAXB class. */
	private static final String CLASS_COL_LABEL = "JAXB Type name:";
	
	/**
	 * Constructor for structure dialog.
	 * @param pluginID the current plugin ID
	 * @param parentShell the parent shell
	 * @param serviceFile the legacy web service descriptor file
	 * @param structure the structure being edited
	 */
	public LegacyStructureDialog(
			final String pluginID,
			final Shell parentShell,
			final IFile serviceFile,
			final CixsStructure structure) {
		super(parentShell);
		mPluginID = pluginID;
		mServiceFile = serviceFile;
		mStructure = structure;
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

		initJaxbPackageCombo(area);
		initJaxbTypeCombo(area);
		WidgetCreator.createLabel(area, CONTAINER_LABEL);
		mCicsContainerText = WidgetCreator.createText(area,
				mStructure.getCicsContainer());
		mCicsContainerText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
			}
		});
		
		
		try {
			createPackageList();
			loadPackageCombos();
		} catch (JavaModelException e) {
			LegstarReport.throwCoreException(e, mPluginID);
		}
	}
	
	/**
	 * Create the JAXB package combo.
	 * @param area the parent composite
	 */
	private void initJaxbPackageCombo(final Composite area) {
		WidgetCreator.createLabel(area, PACKAGE_COL_LABEL);
		mJaxbPackageCombo = WidgetCreator.createCombo(area);
		mJaxbPackageCombo.addSelectionListener(
				new SelectionListener() {
			public void widgetDefaultSelected(final SelectionEvent e) {
				packageListChanged(mJaxbPackageCombo,
						mJaxbTypeList,
						mStructure.getJaxbType());
			}
			public void widgetSelected(final SelectionEvent e) {
				packageListChanged(mJaxbPackageCombo,
						mJaxbTypeList,
						mStructure.getJaxbType());
			}
		});
	}
	
	/**
	 * Create the JAXB type combo.
	 * @param area the parent composite
	 */
	private void initJaxbTypeCombo(final Composite area) {
		WidgetCreator.createLabel(area, CLASS_COL_LABEL);
		mJaxbTypeList = WidgetCreator.createList(area);
		mJaxbTypeList.addSelectionListener(new SelectionListener() {
			public void widgetDefaultSelected(final SelectionEvent e) {
				jaxbTypeListChanged();
			}
			public void widgetSelected(final SelectionEvent e) {
				jaxbTypeListChanged();
			}
		});
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
			mJaxbPackageCombo.add(jaxbPkg);
		}
		/* Make sure an item shows up in the text box */
		if (mPkgMap.size() > 0) {
			int i = 0;
			if (mStructure.getJaxbPackageName() != null) {
				i = mJaxbPackageCombo.indexOf(mStructure.getJaxbPackageName());
			}
			if (i > 0) {
				mJaxbPackageCombo.select(i);
			} else {
				mJaxbPackageCombo.select(0);
			}
			packageListChanged(mJaxbPackageCombo,
					mJaxbTypeList,
					mStructure.getJaxbType());
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
	 * @param typeList the input or output type list box
	 * @param type the current input or output type
	 */
	private void packageListChanged(
			final Combo pkgCombo,
			final List typeList,
			final String type) {
		IPackageFragment pkgf = mPkgMap.get(
				pkgCombo.getText().trim() + "."  + BIND_FRAG);
		if (pkgf != null) {
			java.util.List < String > typesList = classList(pkgf);
			typeList.removeAll();
			for (int i = 0; i < typesList.size(); i++) {
				/* Strip the binding suffix to show JAXB classes names */
				String bindType = typesList.get(i);
				String jaxbType = bindType.substring(
						0, bindType.indexOf(BIND_SUFFIX, 0));
				typeList.add(jaxbType);
			}
			if (typesList.size() > 0) {
				int i = 0;
				if (type != null) {
					i = typeList.indexOf(type);
				}
				if (i > 0) {
					typeList.select(i);
				} else {
					typeList.select(0);
				}
			}
		}
	}
	
	
	/**
	 * User selected an JAXB type.
	 */
	private void jaxbTypeListChanged() {
		if (mJaxbTypeList.getSelectionCount() > 0) {
			mStructure.setJaxbType(mJaxbTypeList.getSelection()[0]);
		}
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
		mStructure.setCicsContainer(mCicsContainerText.getText().trim());
		mStructure.setJaxbPackageName(mJaxbPackageCombo.getText().trim());
		jaxbTypeListChanged();
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
		IJavaProject jproject = JavaCore.create(mServiceFile.getProject());
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
		Collections.sort(classes);
		return classes;
	}

	/**
	 * Ensures that all text fields are set.
	 * @return true if data is valid
	 */
	private boolean validDialogData() {
		
		/* A JAXB type must be selected */
		if (mJaxbTypeList.getItemCount() != 0 
				&& mJaxbTypeList.getSelectionCount() == 0) {
			errorDialog(getShell(), "You must select an input type");
			return false;
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
	    ErrorDialog.openError(shell, "Structure Error", null, status); 
    } 

	/**
	 * @return structure being edited
	 */
	public final CixsStructure getStructure() {
		return mStructure;
	}
	
	/**
	 * @param structure structure being edited
	 */
	public final void setStructure(
			final CixsStructure structure) {
		this.mStructure = structure;
	}

}

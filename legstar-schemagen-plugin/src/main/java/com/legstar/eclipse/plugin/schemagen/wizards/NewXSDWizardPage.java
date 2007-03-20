/*******************************************************************************
 * LegStar legacy Web-enablement .
 * Copyright (C)  2007 LegSem
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301  USA
 * 
 *     
 *****************************************************************************/
package com.legstar.eclipse.plugin.schemagen.wizards;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.ContainerSelectionDialog;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import com.legstar.eclipse.plugin.schemagen.Activator;
import com.legstar.eclipse.plugin.schemagen.SchemagenPreferences;

/**
 * The "New" wizard page allows setting the container for the XML schema as well
 * as the file name. The page will only accept file name without the extension
 * OR with the extension that matches the expected one (xsd).
 */

/**
 * @author Fady
 *
 */
public class NewXSDWizardPage extends WizardPage {

	/** Set of common preferences. */
	private SchemagenPreferences mSchemagenPref;
	
	/** The container field. */
	private Text mContainerText;

	/** The XML schema file name. */
	private Text mXsdFileText;

	/** The target namespace for the XML schema. */
	private Text mNamespaceText;

	/** The current selection. */
	private ISelection mSelection;

	/** Page name. */
	private static final String PAGE_NAME =
		"NewXSDWizardPage";
	
	/** Page description text. */
	private static final String PAGE_TITLE =
		"Target XML Schema";
	
	/** Page description text. */
	private static final String PAGE_DESC =
		"Describe the target XML Schema";
	
	/** Page image location. */
	private static final String PAGE_IMG = "icons/legsemlogo.jpg";
	
	/** XML Schema files extension. */
	private static final String XSD_EXTENSION = "xsd";

	/** Container label text. */
	private static final String CONTAINER_LABEL = "&Container:";

	/** Browse button label text. */
	private static final String BROWSE_LABEL = "Browse...";
	
	/** XML Schema file name label text. */
	private static final String FILE_NAME_LABEL = "&File name:";
	
	/** XML Schema namespace label text. */
	private static final String NAMESPACE_LABEL = "&Namespace:";
	
	/** Browse dialog title. */
	private static final String BROWSE_DIALOG_TITLE =
		"Select new file container";
	
	/** No containers error message. */
	private static final String NO_CONTAINERS_MSG =
		"File container must be specified";
	
	/** Container must exist error message. */
	private static final String CONTAINER_MUST_EXIST_MSG =
		"File container must exist";
	
	/** Project must be writable error message. */
	private static final String PROJECT_NOT_WRITABLE_MSG =
		"Project must be writable";
	
	/** No file name error message. */
	private static final String NO_FILE_NAME_MSG =
		"File name must be specified";
	
	/** Invalid file name error message. */
	private static final String INVALID_FILE_NAME_MSG =
		"File name must be valid";
	
	/** Invalid extension error message. */
	private static final String INVALID_EXTENSION_MSG =
		"File extension must be \"xsd\"";
	
	/** No namespace error message. */
	private static final String NO_NAMESPACE_MSG =
		"Namespace must be specified";
	
	/**
	 * Constructor for NewXSDWizardPage.
	 * 
	 * @param selection the current selection
	 */
	public NewXSDWizardPage(
			final ISelection selection) {
		super(PAGE_NAME);
		setTitle(PAGE_TITLE);
		setDescription(PAGE_DESC);
		mSelection = selection;
		ImageDescriptor image =
            AbstractUIPlugin.
                imageDescriptorFromPlugin(Activator.PLUGIN_ID, PAGE_IMG);
        setImageDescriptor(image);
        mSchemagenPref = new SchemagenPreferences();
	}

	/**
	 * @see IDialogPage#createControl(Composite)
     * @param parent the parent composite
	 */
	public final void createControl(
			final Composite parent) {
		Composite container = new Composite(parent, SWT.NULL);
		GridLayout layout = new GridLayout();
		container.setLayout(layout);
		layout.numColumns = 3;
		layout.verticalSpacing = 9;
		Label label = new Label(container, SWT.NULL);
		label.setText(CONTAINER_LABEL);

		mContainerText = new Text(container, SWT.BORDER | SWT.SINGLE);
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);

		mContainerText.setLayoutData(gd);
		mContainerText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});

		Button button = new Button(container, SWT.PUSH);
		button.setText(BROWSE_LABEL);
		button.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				handleBrowse();
			}
		});
		label = new Label(container, SWT.NULL);
		label.setText(FILE_NAME_LABEL);

		mXsdFileText = new Text(container, SWT.BORDER | SWT.SINGLE);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		mXsdFileText.setLayoutData(gd);
		mXsdFileText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});

		label = new Label(container, SWT.NULL);
		label.setText("");
		label = new Label(container, SWT.NULL);
		label.setText(NAMESPACE_LABEL);

		mNamespaceText = new Text(container, SWT.BORDER | SWT.SINGLE);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		mNamespaceText.setLayoutData(gd);
		mNamespaceText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});
		initialize();
		setControl(container);
	}

	/**
	 * Tests if the current workbench selection is a suitable container to use.
	 */

	private void initialize() {
		if (mSelection != null && !mSelection.isEmpty()
				&& mSelection instanceof IStructuredSelection) {
			IStructuredSelection ssel = (IStructuredSelection) mSelection;
			if (ssel.size() > 1) {
				return;
			}
			Object obj = ssel.getFirstElement();
			if (obj instanceof IResource) {
				IContainer container;
				if (obj instanceof IContainer) {
					container = (IContainer) obj;
				} else {
					container = ((IResource) obj).getParent();
				}
				mContainerText.setText(container.getFullPath().toString());
			}
		}
	}

	/**
	 * Uses the standard container selection dialog to choose the new value for
	 * the container field.
	 */

	private void handleBrowse() {
		ContainerSelectionDialog dialog = new ContainerSelectionDialog(
				getShell(), ResourcesPlugin.getWorkspace().getRoot(), false,
				BROWSE_DIALOG_TITLE);
		if (dialog.open() == ContainerSelectionDialog.OK) {
			Object[] result = dialog.getResult();
			if (result.length == 1) {
				mContainerText.setText(((Path) result[0]).toString());
			}
		}
	}

	/**
	 * Ensures that both text fields are set.
	 */

	private void dialogChanged() {
		if (getContainerName() == null || getContainerName().length() == 0) {
			updateStatus(NO_CONTAINERS_MSG);
			return;
		}
		IResource container = ResourcesPlugin.getWorkspace().getRoot()
		.findMember(new Path(getContainerName()));
		String fileName = getFileName();

		if (container == null
				|| (container.getType()
						& (IResource.PROJECT | IResource.FOLDER)) == 0) {
			updateStatus(CONTAINER_MUST_EXIST_MSG);
			return;
		}
		if (!container.isAccessible()) {
			updateStatus(PROJECT_NOT_WRITABLE_MSG);
			return;
		}
		if (fileName == null ||  fileName.length() == 0) {
			updateStatus(NO_FILE_NAME_MSG);
			return;
		}
		if (fileName.replace('\\', '/').indexOf('/', 1) > 0) {
			updateStatus(INVALID_FILE_NAME_MSG);
			return;
		}
		int dotLoc = fileName.lastIndexOf('.');
		if (dotLoc != -1) {
			String ext = fileName.substring(dotLoc + 1);
			if (!ext.equalsIgnoreCase(XSD_EXTENSION)) {
				updateStatus(INVALID_EXTENSION_MSG);
				return;
			}
		}

		String namespace = getNamespace();
		if (namespace == null ||  namespace.length() == 0) {
			updateStatus(NO_NAMESPACE_MSG);
			return;
		}

		updateStatus(null);
	}

	/**
	 * Pass on messages.
	 * @param message the text
	 */
	private void updateStatus(final String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}

	/**
	 * @return the container name
	 */
	public final String getContainerName() {
		return mContainerText.getText();
	}

	/**
	 * Update the container name.
	 * @param containerName the new name to set
	 */
	public final void setContainerName(final String containerName) {
		mContainerText.setText(containerName);
	}

	/**
	 * @return the container name
	 */
	public final String getFileName() {
		return mXsdFileText.getText();
	}

	/**
	 * Update the file name.
	 * @param fileName the new name to set
	 */
	public final void setFileName(final String fileName) {
		mXsdFileText.setText(fileName);
	}

	/**
	 * @return the container name
	 */
	public final String getNamespace() {
		return mNamespaceText.getText();
	}

	/**
	 * Update the namespace.
	 * @param namespace the new namespace to set
	 */
	public final void setNamespace(final String namespace) {
		mNamespaceText.setText(namespace);
	}
	
	/**
	 * Initializes this page based on the import cobol page content.
	 * @param cobolPage the cobol page
	 */
	public final void initFieldsFromCobolPage(
			final ImportCobolWizardPage cobolPage) {
		setFileName(
				cobolPage.getCobolName().toLowerCase() + '.' + XSD_EXTENSION);
		setNamespace(
				mSchemagenPref.getNsPrefix()
				+ "/" + cobolPage.getCobolName().toLowerCase());
		setContainerName(
				cobolPage.getContainerFullPath().toOSString());
		
	}

}

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
package com.legstar.eclipse.plugin.coxbgen.wizards;

import java.io.IOException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.List;
import org.eclipse.ui.dialogs.ContainerSelectionDialog;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;


/**
 * This basic version of the generation UI will display some of the parameters
 * that drive the generation process and give a chance to cancel the generation
 * if necessary.
 */

public class CoxbGenWizardPage extends WizardPage {

	/** XML schema text field. */
	private Text xsdFileText;
	/** List of complex types from XML schema. . */
	private List mRootNameList;
	/** Target source directory text field. */
	private Text mSrcdirText;
	/** Target binary directory text field. */
	private Text mBindirText;
	/** Target JAXB package name. */
	private String mJaxbPackageName;
	
	/** The current workbench selection. */
	private ISelection mSelection;
	
	/** XML schema file. */
	private IFile mXsdFile;

	/** The XML schema namespace . */
	private static final String XSD_NS = "http://www.w3.org/2001/XMLSchema";
	
	/** The XML schema complex element . */
	private static final String COMPLEX_E = "complexType";

	/** The XML schema complex element name attribute. */
	private static final String NAME_A = "name";
	
	/** The JAXB schema namespace . */
	private static final String JAXB_NS = "http://java.sun.com/xml/ns/jaxb";

	/** The JAXB schema element pointing to target package name. */
	private static final String PACKAGE_E = "package";
	
	/** Page name. */
	private static final String PAGE_NAME =
		"CoxbGenWizardPage";
	
	/** Page description text. */
	private static final String PAGE_TITLE =
		"COXB Generation";
	
	/** Page description text. */
	private static final String PAGE_DESC =
		"Generate COBOL-JAXB binding classes";
	
	/** XML Schema files extension. */
	private static final String XSD_EXTENSION = "xsd";

	/** XML Schema label text. */
	private static final String XSD_LABEL = "&XML schema:";

	/** Root element. */
	private static final String ROOT_LABEL = "&Root elements:";

	/** Browse button label text. */
	private static final String BROWSE_LABEL = "Browse...";
	
	/** Source container label text. */
	private static final String SRC_CONTAINER_LABEL = "&Source container:";
	
	/** Binary container label text. */
	private static final String BIN_CONTAINER_LABEL = "&Binaries container:";
	
	/** No JAXB annotations found error message. */
	private static final String NO_JAXB_ANNOTATIONS_MSG =
		"No JAXB annotation for target package in XML schema file";
	
	/** Browse dialog title. */
	private static final String BROWSE_DIALOG_TITLE =
		"Select new file container";
	
	/** No complex element error message. */
	private static final String NO_COMPLEX_ELEMENT_MSG =
		"No complex element in XML schema file";
	
	/** No XML schema error message. */
	private static final String NO_XML_SCHEMA_MSG =
		"XML schema file name must be specified";
	
	/** Invalid XML schema error message. */
	private static final String INVALID_XML_SCHEMA_MSG =
		"XML schema file name must be valid";
	
	/** Invalid extension error message. */
	private static final String INVALID_EXTENSION_MSG =
		"File extension must be \"xsd\"";
	
	/** No root element selected error message. */
	private static final String NO_ROOT_SELECTED_MSG =
		"Select at least one root element";
	
	
	/** Label for source container. */
	private static final String SOURCE_LABEL = "Source";
	
	/** Label for binaries container. */
	private static final String BINARIES_LABEL = "Binaries";

	/** No containers selected error message. */
	private static final String NO_CONTAINERS_MSG =
		" container must be specified";
	
	/** Container does not exist error message. */
	private static final String CONTAINER_NOEXIST_MSG = " container must exist";
	
	/** Project is not writable error message. */
	private static final String PROJECT_NOT_WRITABLE_MSG =
		"Project must be writable";
	/**
	 * Constructor for CoxbGenWizardPage.
	 * 
	 * @param sel current workbench selection
	 */
	public CoxbGenWizardPage(final ISelection sel) {
		super(PAGE_NAME);
		init(sel);
		mXsdFile = null;
	}

	/**
	 * Constructor for CoxbGenWizardPage from existing XML schema file.
	 * 
	 * @param sel current workbench selection
	 * @param xsdFile XML schema file
	 */
	public CoxbGenWizardPage(final ISelection sel, final IFile xsdFile) {
		super(PAGE_NAME);
		init(sel);
		mXsdFile = xsdFile;
	}
	
	/**
	 * Setup the new page.
	 * @param selection current workbench selection
	 */
	private void init(final ISelection selection) {
		setTitle(PAGE_TITLE);
		setDescription(PAGE_DESC);
		mSelection = selection;
	}

	/**
	 * @see IDialogPage#createControl(Composite)
	 * @param parent composite
	 */
	public final void createControl(final Composite parent) {
		Composite container = new Composite(parent, SWT.NULL);
		GridLayout layout = new GridLayout();
		container.setLayout(layout);
		layout.numColumns = 3;
		layout.verticalSpacing = 9;

		/* XML schema file edit box */
		Label label = new Label(container, SWT.NULL);
		label.setText(XSD_LABEL);

		xsdFileText = new Text(container, SWT.BORDER | SWT.SINGLE);
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		xsdFileText.setLayoutData(gd);
		xsdFileText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});

		label = new Label(container, SWT.NULL);

		/* root name edit box */
		label = new Label(container, SWT.NULL);
		label.setText(ROOT_LABEL);

		mRootNameList = new List(container, SWT.BORDER | SWT.MULTI);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		mRootNameList.setLayoutData(gd);
		mRootNameList.addSelectionListener(new SelectionListener() {
			public void widgetDefaultSelected(final SelectionEvent e) {
				dialogChanged();
			}
			public void widgetSelected(final SelectionEvent e) {
				dialogChanged();
			}
		});

		label = new Label(container, SWT.NULL);

		/* Source container edit box and browse button */
		label = new Label(container, SWT.NULL);
		label.setText(SRC_CONTAINER_LABEL);

		mSrcdirText = new Text(container, SWT.BORDER | SWT.SINGLE);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		
		mSrcdirText.setLayoutData(gd);
		mSrcdirText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});

		Button button = new Button(container, SWT.PUSH);
		button.setText(BROWSE_LABEL);
		button.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				handleBrowseSrc();
			}
		});
	
		/* Binary container edit box and browse button */
		label = new Label(container, SWT.NULL);
		label.setText(BIN_CONTAINER_LABEL);

		mBindirText = new Text(container, SWT.BORDER | SWT.SINGLE);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		
		mBindirText.setLayoutData(gd);
		mBindirText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});

		button = new Button(container, SWT.PUSH);
		button.setText(BROWSE_LABEL);
		button.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				handleBrowseBin();
			}
		});
		
		initialize();
		dialogChanged();
		setControl(container);
	}
	
	/**
	 * Tests if the current workbench selection is a suitable container to use.
	 */
	private void initialize() {
		xsdFileText.setText(mXsdFile.getName());
		initRootNameList();
		locateDirs();
	}
	
	/**
	 * Helper method locates source and binary locations for the project
	 * under which lives the XSD file.
	 */
	private void locateDirs() {
		IResource container = mXsdFile.getParent();
		try {
			IJavaProject jproject = JavaCore.create(mXsdFile.getProject());
			IClasspathEntry[] cpe = jproject.getRawClasspath();
			/* Find the first source location */
			for (int i = 0; i < cpe.length; i++) {
				if (cpe[i].getEntryKind() == IClasspathEntry.CPE_SOURCE) {
					mSrcdirText.setText(cpe[i].getPath().toOSString());
					if (cpe[i].getOutputLocation() != null) {
						mBindirText.setText(cpe[i].
								getOutputLocation().toOSString());
					} else {
						mBindirText.setText(jproject.
								getOutputLocation().toOSString());
					}
					return;
				}
			}
			mSrcdirText.setText(container.getName());
			mBindirText.setText(jproject.getOutputLocation().toOSString());
		} catch (JavaModelException e) {
			mSrcdirText.setText(container.getName());
			mBindirText.setText(mSrcdirText.getText());
		}
	}

	/**
	 * The root name list is created from the list of complex types in
	 * the XML schema.
	 */
	private void initRootNameList() {
		
		if (mXsdFile == null) {
			return;
		}
		mRootNameList.removeAll();
    	DocumentBuilderFactory docBuilderFactory =
    		DocumentBuilderFactory.newInstance();
    	DocumentBuilder docBuilder;
		try {
			docBuilderFactory.setNamespaceAware(true);
			docBuilder = docBuilderFactory.newDocumentBuilder();
			Document doc = docBuilder.parse(mXsdFile.getLocation().toFile());
			
			/* Get the target package name that JAXB will be using */
			NodeList listOfElements = doc.getElementsByTagNameNS(JAXB_NS,
					PACKAGE_E);
			if (listOfElements == null || listOfElements.getLength() == 0) {
				throw (new RuntimeException(NO_JAXB_ANNOTATIONS_MSG));
			}
			mJaxbPackageName = ((Element) listOfElements.item(0)).
			             getAttribute(NAME_A);
			
			/* Now look for all the complex elements */
			listOfElements = doc.getElementsByTagNameNS(XSD_NS,
					COMPLEX_E);
			if (listOfElements == null || listOfElements.getLength() == 0) {
				throw (new RuntimeException(NO_COMPLEX_ELEMENT_MSG));
			}
			for (int i = 0; i < listOfElements.getLength(); i++) {
				mRootNameList.add(((Element) listOfElements.item(i)).
						getAttribute(NAME_A));
			}
			mRootNameList.select(0);

		} catch (ParserConfigurationException e) {
			throw (new RuntimeException(
					"ParserConfigurationException " + e.getMessage()));
		} catch (SAXException e) {
			throw (new RuntimeException(
					"SAXException " + e.getMessage()));
		} catch (IOException e) {
			throw (new RuntimeException(
					"IOException " + e.getMessage()));
		}
		
	}

	/**
	 * Uses the standard container selection dialog to choose the new value for
	 * the source container field.
	 */

	private void handleBrowseSrc() {
		ContainerSelectionDialog dialog = new ContainerSelectionDialog(
				getShell(), ResourcesPlugin.getWorkspace().getRoot(), false,
				BROWSE_DIALOG_TITLE);
		if (dialog.open() == ContainerSelectionDialog.OK) {
			Object[] result = dialog.getResult();
			if (result.length == 1) {
				mSrcdirText.setText(((Path) result[0]).toString());
			}
		}
	}

	/**
	 * Uses the standard container selection dialog to choose the new value for
	 * the binary container field.
	 */

	private void handleBrowseBin() {
		ContainerSelectionDialog dialog = new ContainerSelectionDialog(
				getShell(), ResourcesPlugin.getWorkspace().getRoot(), false,
				BROWSE_DIALOG_TITLE);
		if (dialog.open() == ContainerSelectionDialog.OK) {
			Object[] result = dialog.getResult();
			if (result.length == 1) {
				mBindirText.setText(((Path) result[0]).toString());
			}
		}
	}
	/**
	 * Ensures that all text fields are set.
	 */

	private void dialogChanged() {
		
		/* Validate the XML schema file name */
		String fileName = getXsdFile().getName();
		if (fileName.length() == 0) {
			updateStatus(NO_XML_SCHEMA_MSG);
			return;
		}
		if (fileName.replace('\\', '/').indexOf('/', 1) > 0) {
			updateStatus(INVALID_XML_SCHEMA_MSG);
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

		/* Make sure at least one root name is selected */
		String [] rootNames = getRootName();
		if (rootNames == null) {
			updateStatus(NO_ROOT_SELECTED_MSG);
			return;
		}
		
		/* Validate the source directory */
		if (!validateDir(getSrcDirName(), SOURCE_LABEL)) {
			return;
		}
	
		/* Validate the binary directory */
		if (!validateDir(getBinDirName(), BINARIES_LABEL)) {
			return;
		}

		updateStatus(null);
	}
	
	/**
	 * Verifies if a given folder exists and is usable.
	 * @param dirText the name of the folder
	 * @param dirLabel a label to use for error messages
	 * @return false if folder not valid
	 */
	private boolean validateDir(final String dirText, final String dirLabel) {
		if (dirText.length() == 0) {
			updateStatus(dirLabel + NO_CONTAINERS_MSG);
			return false;
		}
		
		IResource dir = ResourcesPlugin.getWorkspace().getRoot()
			.findMember(new Path(dirText));

		if (dir == null
				|| 
				(dir.getType() & (IResource.PROJECT | IResource.FOLDER)) == 0) {
			updateStatus(dirLabel + CONTAINER_NOEXIST_MSG);
			return false;
		}
		
		if (!dir.isAccessible()) {
			updateStatus(PROJECT_NOT_WRITABLE_MSG);
			return false;
		}
		
		return true;
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
	 * @return the source directory name
	 */
	public final String getSrcDirName() {
		return mSrcdirText.getText();
	}

	/**
	 * @return the the binary directory name
	 */
	public final String getBinDirName() {
		return mBindirText.getText();
	}

	/**
	 * @return the root type name
	 */
	public final String [] getRootName() {
		if (mRootNameList.getSelectionCount() > 0) {
			return mRootNameList.getSelection();
		} else {
			return null;
		}
	}

	/**
	 * @return the XML Schema file
	 */
	public final IFile getXsdFile() {
		return mXsdFile;
	}

	/**
	 * @return the target JAXB package name
	 */
	public final String getJaxbPackageName() {
		return mJaxbPackageName;
	}

	/**
	 * @return the selection
	 */
	public final ISelection getSelection() {
		return mSelection;
	}
	
}

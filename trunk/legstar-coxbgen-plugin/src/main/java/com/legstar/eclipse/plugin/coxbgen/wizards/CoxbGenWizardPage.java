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
import org.eclipse.jface.resource.ImageDescriptor;
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
import org.eclipse.ui.plugin.AbstractUIPlugin;
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
	private List mJaxbRootClassNamesList;

	/** Target source directory for generated sources. */
	private Text mTargetSrcDirText;

	/** Target JAXB package name. */
	private String mJaxbPackageName;

	/** The JAXB suffix optionally appended to Xsd complex type names to derive
	 *  a JAXB class name. */
	private String mJaxbTypeNameSuffix;

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

	/** The JAXB schema element giving the type name suffix. */
	private static final String JAXB_TYPENAME_E = "typeName";

	/** The JAXB schema attribute giving the type name suffix. */
	private static final String JAXB_TYPENAME_SUFFIX_A = "suffix";

	/** Page name. */
	private static final String PAGE_NAME =
		"CoxbGenWizardPage";

	/** Page description text. */
	private static final String PAGE_TITLE =
		"COXB Generation";

	/** Page description text. */
	private static final String PAGE_DESC =
		"Select root elements and destination for generated binding classes";

	/** XML Schema label text. */
	private static final String XSD_LABEL = "&XML schema:";

	/** Root element. */
	private static final String ROOT_LABEL = "&Root elements:";

	/** Browse button label text. */
	private static final String BROWSE_LABEL = "Browse...";

	/** Target source folder label text. */
	private static final String TARGET_SRCDIR_LABEL = "&Target source folder:";

	/** No JAXB annotations found error message. */
	private static final String NO_JAXB_ANNOTATIONS_MSG =
		"No JAXB annotation for target package in XML schema file";

	/** Browse dialog title. */
	private static final String BROWSE_DIALOG_TITLE =
		"Select new file container";

	/** No complex element error message. */
	private static final String NO_COMPLEX_ELEMENT_MSG =
		"No complex element in XML schema file";

	/** No root element selected error message. */
	private static final String NO_ROOT_SELECTED_MSG =
		"Select at least one root element";


	/** Target source folder is not valid error message. */
	private static final String INVALID_SRC_FOLDER_MSG =
		"Source folder is not a valid Java project source folder";

	/**
	 * Constructor for CoxbGenWizardPage from existing XML schema file.
	 * By default, the target project is the Xsd file containing project.
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
		ImageDescriptor image =
            AbstractUIPlugin.
                imageDescriptorFromPlugin(
                		com.legstar.eclipse.plugin.common.Activator.PLUGIN_ID,
                		com.legstar.eclipse.plugin.common.Activator.LOGO_IMG);
		setImageDescriptor(image);
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
		setControl(container);

		/* XML schema file edit box */
		Label label = new Label(container, SWT.NULL);
		label.setText(XSD_LABEL);

		xsdFileText = new Text(container,
				SWT.BORDER | SWT.SINGLE | SWT.READ_ONLY);
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		gd.horizontalSpan = 2;
		xsdFileText.setLayoutData(gd);
		xsdFileText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});


		/* root name edit box */
		label = new Label(container, SWT.NULL);
		label.setText(ROOT_LABEL);
		label.setLayoutData(new GridData(GridData.VERTICAL_ALIGN_BEGINNING));

		mJaxbRootClassNamesList = new List(
				container, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
		gd = new GridData(GridData.FILL_HORIZONTAL);
		gd.heightHint = 100;
		gd.horizontalSpan = 2;
		mJaxbRootClassNamesList.setLayoutData(gd);
		mJaxbRootClassNamesList.addSelectionListener(new SelectionListener() {
			public void widgetDefaultSelected(final SelectionEvent e) {
				dialogChanged();
			}
			public void widgetSelected(final SelectionEvent e) {
				dialogChanged();
			}
		});
		mJaxbRootClassNamesList.setFocus();

		/* Target source folder edit box and browse button */
		label = new Label(container, SWT.NULL);
		label.setText(TARGET_SRCDIR_LABEL);

		mTargetSrcDirText = new Text(container, SWT.BORDER | SWT.SINGLE);
		gd = new GridData(GridData.FILL_HORIZONTAL);

		mTargetSrcDirText.setLayoutData(gd);
		mTargetSrcDirText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});

		Button button = new Button(container, SWT.PUSH);
		button.setText(BROWSE_LABEL);
		button.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				handleBrowseTargetSrcDir();
			}
		});

		initContents();
		dialogChanged();
	}

	/**
	 * Tests if the current workbench selection is a suitable container to use.
	 */
	private void initContents() {
		xsdFileText.setText(mXsdFile.getName());
		initTargetSrcDir();
		initRootNameList();
	}

	/**
	 * Initially, try to set the target src dir as the first source directory of
	 * the project containing the Xsd file. If that project is not a Java 
	 * project leave the field not initialized.
	 */
	private void initTargetSrcDir() {
		try {
			IJavaProject jproject = JavaCore.create(mXsdFile.getProject());
			if (jproject == null) {
				return;
			}
			IClasspathEntry[] cpe = jproject.getRawClasspath();
			/* Find the first source location */
			for (int i = 0; i < cpe.length; i++) {
				if (cpe[i].getEntryKind() == IClasspathEntry.CPE_SOURCE) {
					mTargetSrcDirText.setText(cpe[i].getPath().toOSString());
					return;
				}
			}
		} catch (JavaModelException e) {
			return;
		}
	}

	/**
	 * Check if a relative path name is a valid java source directory.
	 * @param relativePathName the path name
	 * @return true if this is a valid java source folder
	 */
	private boolean isJavaSrcDir(final String relativePathName) {
		IResource resource = ResourcesPlugin.getWorkspace().getRoot()
		.findMember(new Path(relativePathName));
		if (resource != null) {
			IJavaProject jproject = JavaCore.create(resource.getProject());
			if (jproject != null) {
				try {
					IClasspathEntry[] cpe = jproject.getRawClasspath();
					/* Lookup the pathname */
					for (int i = 0; i < cpe.length; i++) {
						if (cpe[i].getEntryKind()
								== IClasspathEntry.CPE_SOURCE) {
							if (relativePathName.equals(
									cpe[i].getPath().toOSString())) {
								return true;
							}
						}
					}
				} catch (JavaModelException e) {
					return false;
				}
			}
		}
		return false;
	}

	/**
	 * Retrieves the output location for java classes associated with a
	 * given java source folder (assumed to be valid).
	 * @param relativePathName the java source folder path name
	 * @return the binaries output location or null if none is found
	 */
	private String getBinDirRelativePathName(final String relativePathName) {
		IResource resource = ResourcesPlugin.getWorkspace().getRoot()
		.findMember(new Path(relativePathName));
		IJavaProject jproject = JavaCore.create(resource.getProject());
		try {
			IClasspathEntry[] cpe = jproject.getRawClasspath();
			/* Lookup the pathname */
			for (int i = 0; i < cpe.length; i++) {
				if (cpe[i].getEntryKind()
						== IClasspathEntry.CPE_SOURCE) {
					if (relativePathName.equals(
							cpe[i].getPath().toOSString())) {
						if (cpe[i].getOutputLocation() != null) {
							return cpe[i].
							getOutputLocation().toOSString();
						} else {
							return jproject.
							getOutputLocation().toOSString();
						}
					}
				}
			}
			return null;
		} catch (JavaModelException e) {
			return null;
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
		mJaxbRootClassNamesList.removeAll();
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

			/* See if there is a suffix to use for complex types */
			fetchTypeNameSuffix(doc);

			/* Now look for all the complex elements */
			listOfElements = doc.getElementsByTagNameNS(XSD_NS,
					COMPLEX_E);
			if (listOfElements == null || listOfElements.getLength() == 0) {
				throw (new RuntimeException(NO_COMPLEX_ELEMENT_MSG));
			}
			for (int i = 0; i < listOfElements.getLength(); i++) {
				mJaxbRootClassNamesList.add(((Element) listOfElements.item(i)).
						getAttribute(NAME_A));
			}
			mJaxbRootClassNamesList.select(0);

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
	 * Retrieve the Jaxb type name suffix attribute if there is one
	 * Assumes a construct like this one.
	 *        <jaxb:nameXmlTransform>
	 *               <jaxb:typeName suffix="Type"/>
	 *          </jaxb:nameXmlTransform>
	 * @param doc the XSD as an XML document 
	 */
	private void fetchTypeNameSuffix(final Document doc) {
		NodeList listOfElements = doc.getElementsByTagNameNS(JAXB_NS,
				JAXB_TYPENAME_E);
		if (listOfElements == null || listOfElements.getLength() == 0) {
			return;
		}
		mJaxbTypeNameSuffix = ((Element) listOfElements.item(0)).
		getAttribute(JAXB_TYPENAME_SUFFIX_A);
	}

	/**
	 * Uses the standard container selection dialog to choose the new value for
	 * the source container field.
	 */

	private void handleBrowseTargetSrcDir() {
		ContainerSelectionDialog dialog = new ContainerSelectionDialog(
				getShell(), ResourcesPlugin.getWorkspace().getRoot(), false,
				BROWSE_DIALOG_TITLE);
		if (dialog.open() == ContainerSelectionDialog.OK) {
			Object[] result = dialog.getResult();
			if (result.length == 1) {
				mTargetSrcDirText.setText(((Path) result[0]).toOSString());
			}
		}
	}

	/**
	 * Ensures that all text fields are set.
	 */
	private void dialogChanged() {

		/* Make sure at least one root name is selected */
		if (getJaxbRootClassNames().size() == 0) {
			((CoxbGenWizard) getWizard()).setCanFinish(false);
			updateStatus(NO_ROOT_SELECTED_MSG);
			return;
		}

		/* Validate the source directory */
		if (!isJavaSrcDir(getSrcDirRelativePathName())) {
			((CoxbGenWizard) getWizard()).setCanFinish(false);
			updateStatus(INVALID_SRC_FOLDER_MSG);
			return;
		}

		((CoxbGenWizard) getWizard()).setCanFinish(true);
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
	 * @return the source directory name
	 */
	public final String getSrcDirRelativePathName() {
		return mTargetSrcDirText.getText();
	}

	/**
	 * @return the the binary directory name
	 */
	public final String getBinDirRelativePathName() {
		return getBinDirRelativePathName(getSrcDirRelativePathName());
	}

	/**
	 * @return the root type name
	 */
	public final java.util.List < String > getJaxbRootClassNames() {
		java.util.List < String > result = new java.util.ArrayList < String >();
		for (String className : mJaxbRootClassNamesList.getSelection()) {
			result.add(className);
		}
		return result;
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

	/**
	 * @return the JAXB suffix optionally appended to Xsd complex type names to
	 *  derive a JAXB class name
	 */
	public final String getJaxbTypeNameSuffix() {
		return mJaxbTypeNameSuffix;
	}

	/**
	 * @param jaxbTypeNameSuffix the JAXB suffix optionally appended to Xsd
	 *  complex type names to derive a JAXB class name
	 */
	public final void setJaxbTypeNameSuffix(
			final String jaxbTypeNameSuffix) {
		mJaxbTypeNameSuffix = jaxbTypeNameSuffix;
	}

}

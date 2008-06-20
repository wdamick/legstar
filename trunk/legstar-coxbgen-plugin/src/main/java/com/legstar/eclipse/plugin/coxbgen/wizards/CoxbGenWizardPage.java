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

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.ArrayList;

import javax.xml.transform.stream.StreamSource;

import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaAnnotation;
import org.apache.ws.commons.schema.XmlSchemaAppInfo;
import org.apache.ws.commons.schema.XmlSchemaCollection;
import org.apache.ws.commons.schema.XmlSchemaComplexType;
import org.apache.ws.commons.schema.XmlSchemaElement;
import org.apache.ws.commons.schema.XmlSchemaObject;
import org.apache.ws.commons.schema.XmlSchemaObjectCollection;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.List;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.legstar.eclipse.plugin.common.ClasspathInitializer;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizard;
import com.legstar.eclipse.plugin.common.wizards.AbstractWizardPage;
import com.legstar.eclipse.plugin.coxbgen.Activator;
import com.legstar.util.NameUtil;

/**
 * This basic version of the generation UI will display some of the parameters
 * that drive the generation process and give a chance to cancel the generation
 * if necessary.
 */

public class CoxbGenWizardPage extends AbstractWizardPage {

	/** XML schema label field. */
	private Label mXsdFileLabel;

	/** JAXB package name extracted from XML Schema annotations. */
	private Label mXsdJaxbPackageName;

	/** JAXB type name suffix extracted from XML Schema annotations. */
	private Label mXsdTypeNameSuffix;

	/** List of complex types from XML schema. . */
	private List mJaxbRootClassNamesList;

	/** Target source directory for generated sources. */
	private Text mTargetSrcDirText;

	/** Target binaries directory for generated sources. */
	private Label mTargetBinDirLabel;

	/** XML schema file. */
	private IFile mXsdFile;

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

	/** No root element selected error message. */
	private static final String NO_ROOT_SELECTED_MSG =
		"Select at least one root element";

	/** Target source folder is not valid error message. */
	private static final String INVALID_SRC_FOLDER_MSG =
		"Source folder is not a valid Java project source folder";

	/** Unable to setup classpath for the target java project.. */
	private static final String CLASSPATH_SETUP_FAILURE_MSG =
		"Failed to setup classpath for target projet. Message:";

	/**
	 * Constructor for CoxbGenWizardPage from existing XML schema file.
	 * By default, the target project is the Xsd file containing project.
	 * 
	 * @param selection current workbench selection
	 * @param xsdFile XML schema file
	 */
	public CoxbGenWizardPage(
			final IStructuredSelection selection, final IFile xsdFile) {
		super(selection, PAGE_NAME, PAGE_TITLE, PAGE_DESC);
		mXsdFile = xsdFile;
	}

	/** {@inheritDoc} */
	public final void createExtendedControls(final Composite container) {
		/* Reminder of XML Schema being processed */
		/* Data extracted from XML schema annotations */
		Group group = createGroup(container, "XML Schema");
		createLabel(group, "File name:");
		mXsdFileLabel = createLabel(group, "", 2);
		createLabel(group, "JAXB package name:");
		mXsdJaxbPackageName = createLabel(group, "", 2);
		createLabel(group, "JAXB type name suffix:");
		mXsdTypeNameSuffix = createLabel(group, "", 2);

		/* root name edit box */
		createLabel(container, "Available root elements:", 3);
		mJaxbRootClassNamesList = new List(
				container, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
		GridData gd = new GridData(GridData.FILL_HORIZONTAL);
		gd.horizontalSpan = 3;
		gd.heightHint = 250;
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
		createLabel(container, "Target source folder:");
		mTargetSrcDirText = createText(container);
		mTargetSrcDirText.addModifyListener(new ModifyListener() {
			public void modifyText(final ModifyEvent e) {
				dialogChanged();
			}
		});
		createBrowseForContainerButton(container,
				"Select target source folder", mTargetSrcDirText);
		
		createLabel(container, "Target binaries folder:");
		mTargetBinDirLabel = createLabel(container, "", 2);

	}

	/**
	 * Initialize all fields.
	 */
	protected void initContents() {
		try {
			mXsdFileLabel.setText(mXsdFile.getName());
			initRootElements(mXsdFile);
			initTargetSrcDir(mXsdFile);
		} catch (CoreException e) {
			errorDialog(getShell(), PAGE_TITLE + " Error", Activator.PLUGIN_ID,
					"Failed to initialize page", e.getMessage());
		}
	}

	/**
	 * Loads a COBOL-annotated XML Schema and populates related widgets.
	 * @param xsdFile the XML schema file from the workspace
	 * @throws CoreException if loading fails. XML Schema file is probably
	 *  invalid.
	 */
	private void initRootElements(final IFile xsdFile) throws CoreException {
		if (mXsdFile == null) {
			throwCoreException("There is no XML schema file");
		}
		try {
			InputStream is = new FileInputStream(
					xsdFile.getLocation().toFile());
			XmlSchemaCollection schemaCol = new XmlSchemaCollection();
			XmlSchema schema = schemaCol.read(new StreamSource(is), null);
			XmlSchemaAnnotation schemaAnnotation = schema.getAnnotation();
			loadXmlSchemaAnnotation(xsdFile, schemaAnnotation);
			XmlSchemaObjectCollection items = schema.getItems();
			for (int i = 0; i < items.getCount(); i++) {
				XmlSchemaObject obj = items.getItem(i);
				if (obj instanceof XmlSchemaElement) {
					loadXmlSchemaRootElement(xsdFile,
							(XmlSchemaElement) obj);
				}
				if (obj instanceof XmlSchemaComplexType) {
					loadXmlSchemaComplexType(xsdFile,
							(XmlSchemaComplexType) obj);
				}
			}

		} catch (FileNotFoundException e) {
			throwCoreException(e);
		}
	}

	/**
	 * The annotation at the XML Schema level is important. It gives the
	 * target JAXB package name and potentially a type name suffix.
	 * The markup is expected to have the following format:
	 *  &lt;xsd:appinfo&gt;
	 *      &lt;jaxb:schemaBindings&gt;
	 *          &lt;jaxb:package name="com.legstar.test.coxb.MSNSearch"/&gt;
	 *          &lt;jaxb:nameXmlTransform&gt;
	 *              &lt;jaxb:typeName suffix="Type"/&gt;
	 *          &lt;/jaxb:nameXmlTransform&gt;
	 *      &lt;/jaxb:schemaBindings&gt;
	 *  &lt;/xsd:appinfo>
	 * @param xsdFile the XML schema file from the workspace
	 * @param schemaAnnotation the annotation node
	 * @throws CoreException if annotation is invalid
	 */
	private void loadXmlSchemaAnnotation(
			final IFile xsdFile,
			final XmlSchemaAnnotation schemaAnnotation) throws CoreException {
		if (schemaAnnotation == null) {
			throwCoreException("XML Schema file " 
					+ xsdFile.getLocation().toOSString() + " is not annotated");
		}
		for (int i = 0; i < schemaAnnotation.getItems().getCount(); i++) {
			XmlSchemaObject item = schemaAnnotation.getItems().getItem(i);
			if (item instanceof XmlSchemaAppInfo) {
				XmlSchemaAppInfo appInfo = (XmlSchemaAppInfo) item;
				NodeList markup = appInfo.getMarkup();
				for (int j = 0; j < markup.getLength(); j++) {
					Node node = markup.item(j);
					if (node instanceof Element) {
						Element el = (Element) node;
						if (JAXB_NS.equals(el.getNamespaceURI())) {
							if ("schemaBindings".equals(el.getLocalName())) {

								NodeList l = el.getElementsByTagNameNS(
										JAXB_NS, PACKAGE_E);
								if (l.getLength() > 0) {
									mXsdJaxbPackageName.setText(getAttrValue(
											l.item(0), NAME_A));
								}

								l = el.getElementsByTagNameNS(JAXB_NS,
										JAXB_TYPENAME_E);
								if (l.getLength() > 0) {
									mXsdTypeNameSuffix.setText(getAttrValue(
											l.item(0), JAXB_TYPENAME_SUFFIX_A));
								}
								break;
							}

						}
					}
				}
			}
		}
	}
	
	/**
	 * Convenience method to get and attribute value from a node when we are
	 * sure it is an element.
	 * @param node the w3c node
	 * @param attrName the attribute name
	 * @return the attribute value
	 */
	private String getAttrValue(final Node node, final String attrName) {
		return ((Element) node).getAttribute(attrName);
	}

	/**
	 * All complex types are potential root elements. The name displayed is
	 * the expected JAXB Class name including any suffix if requested.
	 * @param xsdFile the XML schema file from the workspace
	 * @param xsdComplexType the complex type
	 */
	private void loadXmlSchemaComplexType(
			final IFile xsdFile, final XmlSchemaComplexType xsdComplexType) {
		String normalizedName = NameUtil.toClassName(xsdComplexType.getName());
		if (getJaxbTypeNameSuffix() != null) {
			normalizedName += getJaxbTypeNameSuffix();
		}
		mJaxbRootClassNamesList.add(normalizedName);
	}

	/**
	 * Root XSD elements are also potential root elements. The name displayed
	 * is the JAXB class name.
	 * @param xsdFile the XML schema file from the workspace
	 * @param xsdElement element
	 */
	private void loadXmlSchemaRootElement(
			final IFile xsdFile, final XmlSchemaElement xsdElement) {
		String normalizedName = NameUtil.toClassName(xsdElement.getName());
		mJaxbRootClassNamesList.add(normalizedName);
	}

	/**
	 * Initially, try to set the target src dir as the first source directory of
	 * the project containing the Xsd file. If that project is not a Java 
	 * project leave the field not initialized.
	 * @param xsdFile the XML schema file from the workspace
	 * @throws CoreException if project is invalid
	 */
	private void initTargetSrcDir(final IFile xsdFile) throws CoreException {
		try {
			IJavaProject jproject = JavaCore.create(xsdFile.getProject());
			if (jproject == null) {
				throwCoreException("XML Schema file " 
						+ xsdFile.getLocation().toOSString()
						+ " belongs to an invalid project");
			}
			IClasspathEntry[] cpe = jproject.getRawClasspath();
			/* Find the first source location */
			for (int i = 0; i < cpe.length; i++) {
				if (cpe[i].getEntryKind() == IClasspathEntry.CPE_SOURCE) {
					mTargetSrcDirText.setText(cpe[i].getPath().toOSString());
					break;
				}
			}
		} catch (JavaModelException e) {
			throwCoreException(e);
		}
	}

	/**
	 * Perform validation on data entered so far.
	 */
	protected void dialogChanged() {

		/* Validate the source directory as a Java project source folder */
		if (!isJavaSrcDir(getSrcDirRelativePathName())) {
			updateStatus(INVALID_SRC_FOLDER_MSG);
			return;
		}

		/* Make sure at least one root name is selected */
		if (getJaxbRootClassNames().size() == 0) {
			updateStatus(NO_ROOT_SELECTED_MSG);
			return;
		}

		/* Make sure the target project has LegStar libraries in its classpath*/
		if (!lookupLegStarLibrary(getTargetJavaProject())) {
			try {
				setupLegStarLibrary(getTargetJavaProject());
			} catch (JavaModelException e) {
				updateStatus(CLASSPATH_SETUP_FAILURE_MSG + e.getMessage());
			}
		}

		((AbstractWizard) getWizard()).setCanFinish(true);
		updateStatus(null);

	}

	/**
	 * Check if a relative path name is a valid java source directory.
	 * Also sets the target binary folder.
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
								if (cpe[i].getOutputLocation() != null) {
									mTargetBinDirLabel.setText(
											cpe[i].getOutputLocation()
											.toOSString());
								} else {
									mTargetBinDirLabel.setText(
											jproject.getOutputLocation()
											.toOSString());
								}
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
	 * The target Java project where binding classes will be generated needs
	 * the LegStar core libraries to successfully compile. What we do here
	 * is that we search the current classpath for any occurrence of the
	 * LegStar library.
	 * @param jproject the target java project
	 * @return true if there is a LegStar user library already on the classpath
	 */
	private boolean lookupLegStarLibrary(final IJavaProject jproject) {
		try {
			IClasspathEntry[] cpe = jproject.getRawClasspath();
			for (int i = 0; i < cpe.length; i++) {
				if (cpe[i].getEntryKind()
						== IClasspathEntry.CPE_CONTAINER) {
					if (cpe[i].getPath().equals(
							new Path(ClasspathInitializer.LIBRARY_NAME))) {
						return true;
					}
				}
			}
			return false;
		} catch (JavaModelException e) {
			return false;
		}
	}

	/**
	 * The target Java project needs all the LegStar libraries on its classpath.
	 * This assumes a classpath initializer did define the LegStar library
	 * container and all what is left to do is to update the project with yet
	 * another classpath entry.
	 * 
	 * @param jproject the target java project
	 * @throws JavaModelException if seting up classpath fails
	 */
	private void setupLegStarLibrary(
			final IJavaProject jproject) throws JavaModelException {
		IClasspathEntry varEntry = JavaCore.newContainerEntry(
				new Path(ClasspathInitializer.LIBRARY_NAME),
				false);

		java.util.List < IClasspathEntry > sourceEntries =
			new ArrayList < IClasspathEntry >();
		for (IClasspathEntry entry : jproject.getRawClasspath()) {
			sourceEntries.add(entry);
		}
		sourceEntries.add(varEntry);
		IClasspathEntry[] entries = (IClasspathEntry[]) sourceEntries.toArray(
				new IClasspathEntry[sourceEntries.size()]);
		jproject.setRawClasspath(entries, null);
	}

	/**
	 * The target source folder is part of a Java project. This is validated
	 * by <code>isJavaSrcDir</code>.
	 * @return the target java project
	 */
	private IJavaProject getTargetJavaProject() {
		IResource resource = ResourcesPlugin.getWorkspace().getRoot()
		.findMember(new Path(getSrcDirRelativePathName()));
		return JavaCore.create(resource.getProject());
	}

	/**
	 * @return the source directory name
	 */
	public final String getSrcDirRelativePathName() {
		return mTargetSrcDirText.getText();
	}

	/**
	 * Retrieves the output location for java classes associated with a
	 * given java source folder (assumed to be valid).
	 * @return the binaries output location
	 */
	public String getBinDirRelativePathName() {
		return mTargetBinDirLabel.getText();
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
		return mXsdJaxbPackageName.getText();
	}

	/**
	 * @return the JAXB suffix optionally appended to Xsd complex type names to
	 *  derive a JAXB class name
	 */
	public final String getJaxbTypeNameSuffix() {
		return mXsdTypeNameSuffix.getText();
	}

}

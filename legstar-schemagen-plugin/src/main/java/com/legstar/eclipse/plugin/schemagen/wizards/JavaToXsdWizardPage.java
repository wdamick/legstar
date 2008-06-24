package com.legstar.eclipse.plugin.schemagen.wizards;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.SelectionDialog;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;

import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jdt.ui.IJavaElementSearchConstants;
import org.eclipse.jdt.ui.ISharedImages;
import org.eclipse.jdt.ui.JavaUI;

import com.legstar.eclipse.plugin.schemagen.Activator;
import com.legstar.eclipse.plugin.schemagen.Messages;
import com.legstar.eclipse.plugin.schemagen.util.JavaClass;
import com.legstar.eclipse.plugin.schemagen.viewers.JavaClassSorter;

/**
 * This wizard page allows users to create a list of java classes
 * that will be introspected to generate an XML schema.
 */
public class JavaToXsdWizardPage extends AbstractToXsdWizardPage {

	/** Table that is wrappered in a table viewer. */
	private Table mTable;

	/** The table column names. */
	private String[] mColumnNames = {"Java Project", "Class name"};

	/** Table view of the selected java classes. */
	private TableViewer mJavaClassesTableViewer;
	
	/** The actual content displayed by the viewer. */
	private List < JavaClass > mModel = new ArrayList < JavaClass >();

	/** The table viewer height. */
	private static final int TABLE_VIEWER_HEIGHT = 200;

	/**
	 * Constructs the wizard page.
	 * @param initialSelection the workbench current selection
	 */
	public JavaToXsdWizardPage(final IStructuredSelection initialSelection) {
		super(initialSelection,
				"JavaToXsdWizardPage",
				Messages.java_To_xsd_wizard_page_title,
				Messages.java_To_xsd_wizard_page_description);
	}

	/** {@inheritDoc} */
	@Override
	protected void createExtendedControls(final Composite container) {
		createSelectJavaClassesLink(container);
		createJavaClassesTableViewer(container);
	}

	/**
	 * This link will popup the resource selection dialog.
	 * @param container the parent container
	 */
	private void createSelectJavaClassesLink(final Composite container) {
		createHyperlink(container,
				Messages.selection_dialog_title,
				JavaUI.getSharedImages().getImage(
						ISharedImages.IMG_OBJS_CLASS),
						new HyperlinkAdapter() {
			public void linkActivated(final HyperlinkEvent e) {
				addJavaClasses();
			}
		});
	}

	/**
	 * The composite widget presenting the currently selected java classes.
	 * @param container the parent container
	 */
	private void createJavaClassesTableViewer(final Composite container) {
		createTable(container);
		mJavaClassesTableViewer = new TableViewer(mTable);
		mJavaClassesTableViewer.setUseHashlookup(true);
		mJavaClassesTableViewer.setColumnProperties(mColumnNames);
		mJavaClassesTableViewer.setSorter(
				new JavaClassSorter(JavaClassSorter.JAVAPROJECT));
		mJavaClassesTableViewer.setLabelProvider(
				new JavaClassesTableLabelProvider());
		mJavaClassesTableViewer.setContentProvider(
				new JavaClassesTableContentProvider());
		mJavaClassesTableViewer.setInput(mModel);

		final Button removeButton = new Button(container, SWT.NONE);
		removeButton.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				IStructuredSelection selection = (IStructuredSelection)
				mJavaClassesTableViewer.getSelection();
				for (Iterator < ? > iterator = selection.iterator();
				iterator.hasNext();) {
					JavaClass element = (JavaClass) iterator.next();
					removeJavaClass(element);
				}
				dialogChanged();

			}
		});
		removeButton.setText(Messages.remove_button_label);
		GridData gridData = new GridData(GridData.HORIZONTAL_ALIGN_END);
		gridData.horizontalSpan = LAYOUT_COLUMNS;
		removeButton.setLayoutData(gridData);

	}

	/**
	 * Create the internal viewer table.
	 * @param container the parent container
	 */
	private void createTable(final Composite container) {
		int style = SWT.MULTI | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL
		| SWT.FULL_SELECTION | SWT.HIDE_SELECTION;

		mTable = new Table(container, style);

		GridData gridData = new GridData(GridData.FILL_BOTH);
		gridData.grabExcessVerticalSpace = true;
		gridData.horizontalSpan = LAYOUT_COLUMNS;
		gridData.heightHint = TABLE_VIEWER_HEIGHT;
		mTable.setLayoutData(gridData);		

		mTable.setLinesVisible(true);
		mTable.setHeaderVisible(true);

		/* First column in the Java project */
		TableColumn column = new TableColumn(mTable, SWT.LEFT, 0);		
		column.setText(mColumnNames[0]);
		column.setWidth(120);
		/* Add listener to column so tasks are sorted by class name when
		 * clicked */ 
		column.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				mJavaClassesTableViewer.setSorter(
						new JavaClassSorter(JavaClassSorter.JAVAPROJECT));
			}
		});

		/* 2nd column is the java class name */
		column = new TableColumn(mTable, SWT.LEFT, 1);
		column.setText(mColumnNames[1]);
		column.setWidth(400);
		/* Add listener to column so tasks are sorted by class name when
		 * clicked */ 
		column.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				mJavaClassesTableViewer.setSorter(
						new JavaClassSorter(JavaClassSorter.JAVACLASS));
			}
		});

	}

	/**
	 * Java classes appear in the table viewer as a descriptive string.
	 */
	public class JavaClassesTableLabelProvider extends LabelProvider
	implements ITableLabelProvider {

		/** {@inheritDoc} */
		public Image getColumnImage(
				final Object element, final int columnIndex) {
			return null;
		}

		/** {@inheritDoc} */
		public String getColumnText(
				final Object element, final int columnIndex) {
			JavaClass jClass = (JavaClass) element;
			switch (columnIndex) {
			case 0:
				return jClass.javaProject.getProject().getName();
			case 1 :
				return jClass.className;
			default :
				return ""; 	
			}
		}
	}
	
	/**
	 * The content provider bridging the model with the viewer.
	 */
	public class JavaClassesTableContentProvider
			implements IStructuredContentProvider {

		/** {@inheritDoc} */
		public Object[] getElements(final Object parent) {
			return mModel.toArray();
		}

		/** {@inheritDoc} */
		public void dispose() {
			
		}

		/** {@inheritDoc} */
		public void inputChanged(final Viewer viewer,
				final Object oldInput, final Object newInput) {
			
		}
		
	}

	/**
	 * Present a type selection dialog starting from the workspace root.
	 * This will allow user to select java classes from more than one project.
	 */
	private void addJavaClasses() {
        try {
            SelectionDialog dialog = JavaUI.createTypeDialog(
                    getShell(),
                    PlatformUI.getWorkbench().getProgressService(),
                    SearchEngine.createWorkspaceScope(),
                    IJavaElementSearchConstants.CONSIDER_CLASSES,
                    true);
            if (Window.OK == dialog.open()) {
                Object[] results = dialog.getResult();
                if (results != null && results.length > 0) {
            		for (Object obj : results) {
            			if (obj instanceof IType) {
            				if (!addJavaClass((IType) obj)) {
            					break;
            				}
            			}
            		}
                }
        		dialogChanged();
            }
        } catch (JavaModelException e1) {
            errorDialog(getShell(),
            		Messages.selection_error_dialog_title,
            		Activator.PLUGIN_ID,
                    Messages.selection_dialog_init_failure_short_msg,
                    NLS.bind(Messages.selection_dialog_init_failure_long_msg,
                    		e1.getMessage()));
            logCoreException(e1, Activator.PLUGIN_ID);
        }
	}

	/**
	 * Add a new type as a java class.
	 * @param type the java type
	 * @return true if add succeeded
	 */
	private boolean addJavaClass(final IType type) {
		JavaClass jClass = new JavaClass(
				type.getFullyQualifiedName(), type.getJavaProject());
		addJavaClass(jClass);
		return true;
	}
	
	/**
	 * Adds the class to the model and makes sure the viewer is aware.
	 * @param jClass the new java class
	 */
	private void addJavaClass(final JavaClass jClass) {
		if (mModel.contains(jClass)) {
			return;
		}
		mModel.add(jClass);
		mJavaClassesTableViewer.add(jClass);
	}

	/**
	 * Remove a class from the model and makes sure the viewer is aware.
	 * @param jClass the new java class
	 */
	private void removeJavaClass(final JavaClass jClass) {
		if (!mModel.contains(jClass)) {
			return;
		}
		mModel.remove(jClass);
		mJavaClassesTableViewer.remove(jClass);
	}

	/**
	 * @return a list of all fully qualified selected class names
	 */
	public List < String > getSelectedClassNames() {
		List < String > selectedClassNames = new ArrayList < String >();
		for (int i = 0; i <  mJavaClassesTableViewer.getTable().getItemCount();
		i++) {
			JavaClass jClass = (JavaClass)
			mJavaClassesTableViewer.getElementAt(i);
			selectedClassNames.add(jClass.className);
		}
		return selectedClassNames;
	}

	/**
	 * Given a set of selected java classes, this will create a list of path
	 * element locations from their projects classpath entries.
	 * @return a list of path element locations
	 */
	public List < String > getSelectedPathElementsLocations() {
		List < String > selectedPathElementsLocations =
			new ArrayList < String >();
		List < IJavaProject > mProcessed = new ArrayList < IJavaProject >();
		for (int i = 0; i <  mJavaClassesTableViewer.getTable().getItemCount();
		i++) {
			JavaClass jClass = (JavaClass)
			mJavaClassesTableViewer.getElementAt(i);
			IJavaProject javaProject = jClass.javaProject;
			if (!mProcessed.contains(javaProject)) {
				addPathElements(selectedPathElementsLocations, javaProject);
			}
		}
		return selectedPathElementsLocations;
	}

	/**
	 * Extract classpath entries from given java project and store them as
	 * path elements in a list.
	 * @param selectedPathElementsLocations list of path elements locations
	 * @param javaProject the input java project
	 */
	private void addPathElements(
			final List < String > selectedPathElementsLocations,
			final IJavaProject javaProject) {
		try {
			IClasspathEntry[] classPathEntries = javaProject.getRawClasspath();
			addPathElements(selectedPathElementsLocations,
					classPathEntries, javaProject);
		} catch (JavaModelException e) {
			errorDialog(getShell(),
					Messages.classpath_init_error_dialog_title,
					Activator.PLUGIN_ID,
					Messages.classpath_init_failure_short_msg,
					NLS.bind(Messages.classpath_init_failure_long_msg,
							javaProject.getElementName(), e.getMessage()));
		}
	}

	/**
	 * Given classpath entries from a java project, populate a list of
	 *  collections.
	 * @param selectedPathElementsLocations the output path locations
	 * @param classPathEntries the java project class path entries
	 * @param javaProject the java project
	 * @throws JavaModelException if invalid classpath
	 */
	private void addPathElements(
			final List < String > selectedPathElementsLocations,
			final IClasspathEntry[] classPathEntries,
			final IJavaProject javaProject) throws JavaModelException {

		IClasspathEntry jreEntry = JavaRuntime.getDefaultJREContainerEntry();
		IPath projectPath = javaProject.getProject().getLocation();

		for (int i = 0; i < classPathEntries.length; i++) {
			IClasspathEntry classpathEntry = classPathEntries[i];
			String pathElementLocation = null;
			switch (classpathEntry.getEntryKind()) {
			case IClasspathEntry.CPE_LIBRARY :
				pathElementLocation = classpathEntry.getPath().toOSString();
				break;
			case IClasspathEntry.CPE_CONTAINER :
				/* No need for the default jre */
				if (classpathEntry.equals(jreEntry)) {
					break;
				}
				/* Resolve container into class path entries */
				IClasspathContainer classpathContainer =
					JavaCore.getClasspathContainer(classpathEntry.getPath(),
							javaProject);
				addPathElements(selectedPathElementsLocations,
						classpathContainer.getClasspathEntries(),
						javaProject);
				break;
			case IClasspathEntry.CPE_VARIABLE :
				pathElementLocation = JavaCore.getResolvedVariablePath(
						classpathEntry.getPath()).toOSString();
				break;
			case IClasspathEntry.CPE_SOURCE :
				/* If source has no specific output, use the project default
				 *  one*/
				IPath outputLocation = classpathEntry.getOutputLocation();
				if (outputLocation == null) {
					outputLocation = javaProject.getOutputLocation();
				}
				pathElementLocation = projectPath.append(
						outputLocation.removeFirstSegments(1)).toOSString();
				break;
			default:
				break;
			}

			if (pathElementLocation != null
					&& !selectedPathElementsLocations.contains(
							pathElementLocation)) {
				selectedPathElementsLocations.add(pathElementLocation);
			}
		}
	}

	/** {@inheritDoc} */
	protected void dialogChanged() {
		if (mJavaClassesTableViewer.getTable().getItemCount() > 0) {
			updateStatus(null);
		} else {
			updateStatus(Messages.no_java_classes_selected_msg);
		}

	}

	/** {@inheritDoc} */
	protected void initContents() {
	}

	/** 
	 * {@inheritDoc}
	 * @see org.eclipse.jface.wizard.WizardPage#getNextPage()
	 *  */
	@Override
	public IWizardPage getNextPage() {
		return null;
	}
}

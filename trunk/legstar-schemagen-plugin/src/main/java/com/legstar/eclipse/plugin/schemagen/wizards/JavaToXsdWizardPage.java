package com.legstar.eclipse.plugin.schemagen.wizards;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.IClasspathContainer;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.dialogs.ResourceSelectionDialog;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;

import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jdt.ui.ISharedImages;
import org.eclipse.jdt.ui.JavaUI;

import com.legstar.eclipse.plugin.schemagen.Activator;
import com.legstar.eclipse.plugin.schemagen.util.JavaClass;

/**
 * This wizard page allows users to create a list of java classes
 * that will be introspected to generate an XML schema.
 */
public class JavaToXsdWizardPage extends AbstractWizardPage {

    /** List view of the selected java classes. */
    private ListViewer mJavaClassesListViewer;

    /** The list viewer height. */
    private static final int LIST_VIEWER_HEIGHT = 200;

    /** No java classes error message. */
    private static final String NO_JAVA_CLASSES_MSG =
        "You must select at least one java class";

    /**
     * Constructs the wizard page.
     * @param initialSelection the workbench current selection
     */
    public JavaToXsdWizardPage(final IStructuredSelection initialSelection) {
        super(initialSelection,
                "JavaToXsdWizardPage",
                "Generate COBOL-annotated XML Schema from Java Classes",
        "Select the java classes to be used for"
        + " COBOL-annotated XML Schema generation");
    }

	/** {@inheritDoc} */
    @Override
    protected void createExtendedControls(final Composite container) {
        createSelectJavaClassesLink(container);
        createJavaClassesListViewer(container);
    }

    /**
     * This link will popup the resource selection dialog.
     * @param container the parent container
     */
    private void createSelectJavaClassesLink(final Composite container) {
        createHyperlink(container,
                "Select Java classes from workspace",
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
    private void createJavaClassesListViewer(final Composite container) {
        mJavaClassesListViewer = new ListViewer(
                container,
                SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL | SWT.BORDER);
        mJavaClassesListViewer.setLabelProvider(
        		new JavaClassesListLabelProvider());
        mJavaClassesListViewer.setContentProvider(new ArrayContentProvider());
        mJavaClassesListViewer.setSorter(new ViewerSorter() {
            public int compare(
                    final Viewer viewer, final Object p1, final Object p2) {
                return ((JavaClass) p1).compare((JavaClass) p2);
            }
        });
        GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
        gridData.horizontalSpan = LAYOUT_COLUMNS;
        gridData.heightHint = LIST_VIEWER_HEIGHT;
        mJavaClassesListViewer.getList().setLayoutData(gridData);

        final Button removeButton = new Button(container, SWT.NONE);
        removeButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(final SelectionEvent e) {
                IStructuredSelection selection = (IStructuredSelection)
                	mJavaClassesListViewer.getSelection();
                for (Iterator < ? > iterator = selection.iterator();
                		iterator.hasNext();) {
                    JavaClass element = (JavaClass) iterator.next();
                    mJavaClassesListViewer.remove(element);
                }
                dialogChanged();

            }
        });
        removeButton.setText("Remove");
        gridData = new GridData(GridData.HORIZONTAL_ALIGN_END);
        gridData.horizontalSpan = LAYOUT_COLUMNS;
        removeButton.setLayoutData(gridData);

    }

    /**
     * Java classes appear in the list viewer as a descriptive string.
     */
    public class JavaClassesListLabelProvider extends LabelProvider {

    	/** {@inheritDoc}
         * @see org.eclipse.jface.viewers.LabelProvider#getImage(
         * java.lang.Object)
         */
        @Override
        public Image getImage(final Object element) {
            return super.getImage(element);
        }

    	/** {@inheritDoc}
         * @see org.eclipse.jface.viewers.LabelProvider#getText(
         * java.lang.Object)
         */
        @Override
        public String getText(final Object element) {
            JavaClass jClass = (JavaClass) element;
            return jClass.toString();
        }
    }

    /**
     * Present a resource selection dialog starting from the workspace root.
     * This will allow user to select java classes from more than one project.
     */
    private void addJavaClasses() {
        IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        ResourceSelectionDialog dialog =
            new ResourceSelectionDialog(getShell(), root,
            "Select Java classes from the workspace");
        dialog.open();
        for (Object obj : dialog.getResult()) {
            if (obj instanceof IResource) {
                if (!addJavaClass((IResource) obj)) {
                    break;
                }
            }
        }
        dialogChanged();
    }

    /**
     * Checks a resource to see if it is a java class and adds it to the class
     * list.
     * @param resource the resource to checks
     * @return true if the java class is successfully added
     */
    private boolean addJavaClass(final IResource resource) {
        IProject project = resource.getProject();
        IJavaProject javaProject = JavaCore.create(project);
        if (javaProject == null) {
            errorDialog(getShell(), "Add Java class error", Activator.PLUGIN_ID,
                    "Selected project is not a java project ",
                    "The project selected " + project.getName()
                    + " may not have a Java nature");
            return false;
        }
        if (resource instanceof IFile) {
            IJavaElement element = JavaCore.create((IFile) resource);
            if (element instanceof ICompilationUnit) {
                IType type = ((ICompilationUnit) element).findPrimaryType();
                JavaClass jClass = new JavaClass(
                		type.getFullyQualifiedName(), javaProject);
                mJavaClassesListViewer.add(jClass);
                mJavaClassesListViewer.refresh(jClass, false);
            }
        }
        return true;
    }

    /**
     * @return a list of all fully qualified selected class names
     */
    public List < String > getSelectedClassNames() {
        List < String > selectedClassNames = new ArrayList < String >();
        for (int i = 0; i <  mJavaClassesListViewer.getList().getItemCount();
        		i++) {
            JavaClass jClass = (JavaClass)
            	mJavaClassesListViewer.getElementAt(i);
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
        for (int i = 0; i <  mJavaClassesListViewer.getList().getItemCount();
        		i++) {
            JavaClass jClass = (JavaClass)
            	mJavaClassesListViewer.getElementAt(i);
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
            errorDialog(getShell(), "Extract classpath error",
            		Activator.PLUGIN_ID,
                    "Selected project has a classpath issue ",
                    "The java project selected " + javaProject.getElementName()
                    + " has generated a JavaModelException " + e);
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
    @Override
    protected void dialogChanged() {
        if (mJavaClassesListViewer.getList().getItemCount() > 0) {
            ((MainWizard) getWizard()).setCanFinish(true);
            updateStatus(null);
        } else {
            updateStatus(NO_JAVA_CLASSES_MSG);
            ((MainWizard) getWizard()).setCanFinish(false);
        }

    }

	/** {@inheritDoc} */
    @Override
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

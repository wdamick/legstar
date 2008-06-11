package com.legstar.eclipse.plugin.cixsmap.dialogs;

import java.util.ArrayList;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.osgi.framework.BundleContext;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceReference;

import com.legstar.eclipse.plugin.cixscom.wizards.ICixsGeneratorWizardLauncher;
import com.legstar.eclipse.plugin.cixsmap.Activator;

/**
 * This dialog dynamically builds a list of available generators for legacy
 * mapping files. User can select a generator from the list. When OK is 
 * pressed, the corresponding generator wizard is launched.
 * This relies on OSGI registration services. This assumes all available
 * generators will be registered on startup.
 *
 */
public class GenerateDialog extends AbstractDialog {

	/** Dialog title. */
	private static final String DIALOG_TITLE = "Select generation target";

	/** Dialog box title. */
	private static final String DIALOG_ERROR_TITLE = "Generation Error";

	/** The legacy mapping file. */
	private IFile mMappingFile;

	/** Presents the list of registered generators. */
	private List mGeneratorsList = null;

	/** List of available component generator wizards. */
	private
	java.util.List < ICixsGeneratorWizardLauncher > mWizardLaunchers;

	/**
	 * Constructor for generation dialog.
	 * @param pluginID the current plugin ID
	 * @param parentShell the parent shell
	 * @param mappingFile the mapping file
	 */
	public GenerateDialog(
			final String pluginID,
			final Shell parentShell,
			final IFile mappingFile) {
		super(parentShell, pluginID);
		mMappingFile = mappingFile;
	}

	/**
	 * This is overridden so we have OK button disabled until the dialog
	 * content is valid.
	 * {@inheritDoc}
	 */
	protected Control createContents(final Composite parent) {
		Control control = super.createContents(parent);
		getButton(IDialogConstants.OK_ID).setEnabled(false);
		return control;
	}

	/** {@inheritDoc}    */
	protected final Control createDialogArea(final Composite parent) {
		parent.getShell().setText(DIALOG_TITLE);
		Composite composite = (Composite) super.createDialogArea(parent);
		try {
			initialize(composite);
		} catch (CoreException e) {
			errorDialog(DIALOG_ERROR_TITLE,
					"Selected file " +  mMappingFile.getName()
					+ " is not a valid mapping file. "
					+ "CoreException: " 
					+ " " + e.getMessage());
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

		mGeneratorsList = createList(area);
		mGeneratorsList.addSelectionListener(
				new SelectionListener() {

					public void widgetDefaultSelected(
							final SelectionEvent arg0) {
						dialogChanged();
					}

					public void widgetSelected(final SelectionEvent arg0) {
						dialogChanged();
					}

				});
		mWizardLaunchers = loadWizardLaunchers();

		initGeneratorsCombo();
	}

	/**
	 * Validate dialog content.
	 */
	private void dialogChanged() {
		if (getButton(IDialogConstants.OK_ID) != null) {
			if (mGeneratorsList.getSelectionCount() > 0) {
				getButton(IDialogConstants.OK_ID).setEnabled(true);
			} else {
				getButton(IDialogConstants.OK_ID).setEnabled(false);
			}
		}
	}

	/**
	 * Populates the list box with the names of available launchers.
	 */
	private void initGeneratorsCombo() {
		for (ICixsGeneratorWizardLauncher launcher : mWizardLaunchers) {
			mGeneratorsList.add(launcher.getName());
		}
	}

	/**
	 * Uses the OSGI service registration mechanism to discover all available
	 * wizard launchers.
	 * @return a list of available wizard launchers
	 */
	private
	java.util.List < ICixsGeneratorWizardLauncher > loadWizardLaunchers() {
		java.util.List < ICixsGeneratorWizardLauncher > wizardLaunchers =
			new ArrayList < ICixsGeneratorWizardLauncher >();
		try {
			BundleContext context =
				Activator.getDefault().getBundle().getBundleContext();
			ServiceReference[] references =
				context.getServiceReferences(
						ICixsGeneratorWizardLauncher.class.getName(),
						null);
			if (references != null) {
				for (ServiceReference reference : references) {
					ICixsGeneratorWizardLauncher launcher =
						(ICixsGeneratorWizardLauncher) context.getService(
								reference);
					wizardLaunchers.add(launcher);
				}
			} else {
				errorDialog(DIALOG_ERROR_TITLE,
						"Unable to find any available component generators");
			}
		} catch (InvalidSyntaxException e) {
			errorDialog(DIALOG_ERROR_TITLE,
					"Exception trying to retrieve available"
					+ " component generators. "
					+ "InvalidSyntaxException: " 
					+ " " + e.getMessage());
		}
		return wizardLaunchers;

	}

	/**
	 * When ok is pressed, we are sure that a launcher is selected from
	 * the list. So all what is left to do it to invoke its start method.
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#okPressed()
	 */
	@Override
	protected final void okPressed() {
		ICixsGeneratorWizardLauncher launcher =
			mWizardLaunchers.get(mGeneratorsList.getSelectionIndex());
		try {
			launcher.startGenerationWizard(mMappingFile);
		} catch (CoreException e) {
			errorDialog(DIALOG_ERROR_TITLE,
					"Exception trying to launch generation wizard:"
					+ launcher.getName()
					+ " CoreException: " 
					+ " " + e.getMessage());
		}
		setReturnCode(OK);
		close();
	}

}

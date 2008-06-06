package com.legstar.eclipse.plugin.schemagen.wizards;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.events.HyperlinkAdapter;
import org.eclipse.ui.forms.events.HyperlinkEvent;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardPage;

/**
 * This wizard page allows users to select and edit the content of
 * a COBOL source. Alternatively, user can copy/paste COBOL code
 * directly and then generate an annotated XML Schema.
 */
public class CobolToXsdWizardPage extends AbstractToXsdWizardPage {

	/** A COBOL fragment. */
	private Text mCobolFragmentText;

	/** No COBOL fragment error message. */
	private static final String NO_COBOL_FRAGMENT_MSG =
		"You must select at least a COBOL fragment or type COBOL code";

	/** A simple ruler to help with COBOL instructions entry. */
	private static final String COBOL_RULE_LABEL_LINE1 =
		"0--------1---------2---------3---------4---------5---------6"
		+ "---------7--    ";

	/**
	 * Constructs the wizard page.
	 * @param initialSelection the workbench current selection
	 */
	public CobolToXsdWizardPage(final IStructuredSelection initialSelection) {
		super(initialSelection,
				"CobolToXsdWizardPage",
				"Generate XML Schema from COBOL fragments",
				"Select the COBOL fragments to be used for"
				+ " COBOL-annotated XML Schema generation");
	}

	/** {@inheritDoc} */
	@Override
	protected void createExtendedControls(final Composite container) {
		createSelectCobolFragmentsLink(container);
		Label lableLine1 = createLabel(
				container, COBOL_RULE_LABEL_LINE1, LAYOUT_COLUMNS);
		mCobolFragmentText = createMultilineTextField(
				container, LAYOUT_COLUMNS);
		FontData defaultFont = new FontData("Courier New", 8, SWT.NORMAL);
		Font font = new Font(container.getDisplay(), defaultFont);
		mCobolFragmentText.setFont(font);
		lableLine1.setFont(font);
		lableLine1.setBackground(
				container.getDisplay().getSystemColor(SWT.COLOR_GRAY));
	}

	/**
	 * This link will popup the resource selection dialog.
	 * @param container the parent container
	 */
	private void createSelectCobolFragmentsLink(final Composite container) {
		createHyperlink(container,
				"Select Cobol fragments from file system",
				PlatformUI.getWorkbench().getSharedImages().getImage(
						ISharedImages.IMG_OBJ_FOLDER),
						new HyperlinkAdapter() {
			public void linkActivated(final HyperlinkEvent e) {
				mCobolFragmentText.setText(selectSingleFileContent(
						"Select a COBOL file"));
			}
		});
	}

	/** {@inheritDoc} */
	@Override
	protected void dialogChanged() {
		if (mCobolFragmentText.getText().length() > 0) {
			((MainWizard) getWizard()).setCanFinish(true);
			updateStatus(null);
		} else {
			updateStatus(NO_COBOL_FRAGMENT_MSG);
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
	 */
	public IWizardPage getNextPage() {
		return null;
	}

	/**
	 * @return the Cobol Fragment Text
	 */
	public final Text getCobolFragmentText() {
		return mCobolFragmentText;
	}
}

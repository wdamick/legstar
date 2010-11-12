package com.legstar.eclipse.plugin.schemagen.preferences;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.ide.IDEEncoding;
import org.eclipse.ui.ide.dialogs.AbstractEncodingFieldEditor;

/**
 * This is a clone of org.eclipse.ui.ide.dialogs.EncodingFieldEditor.
 * The only difference is that we override the setPreferenceStore method
 * so that this becomes usable in a FieldEditorPreferencePage where
 * the dispose method calls setPreferenceStore with null.
 * <p/>
 * Of course we can't override EncodingFieldEditor because its declared final.
 * 
 */
public class FixedEncodingFieldEditor extends AbstractEncodingFieldEditor {
    /**
     * Creates a new encoding field editor with the given preference name, label
     * and parent.
     * 
     * @param name
     *            the name of the preference this field editor works on
     * @param labelText
     *            the label text of the field editor
     * @param groupTitle
     *            the title for the field editor's control. If groupTitle is
     *            <code>null</code> the control will be unlabelled
     *            (by default a {@link Composite} instead of a {@link Group}.
     * @param parent
     *            the parent of the field editor's control
     * @see AbstractEncodingFieldEditor#setGroupTitle(String)
     * @since 3.3
     */
    public FixedEncodingFieldEditor(final String name, final String labelText,
            final String groupTitle, final Composite parent) {
        super();
        init(name, labelText);
        setGroupTitle(groupTitle);
        createControl(parent);
    }

    /**
     * Create a new instance of the receiver on the preference called name
     * with a label of labelText.
     * 
     * @param name
     *            the name of the preference this field editor works on
     * @param labelText
     *            the label text of the field editor
     * @param parent
     *            the parent of the field editor's control
     */
    public FixedEncodingFieldEditor(final String name, final String labelText,
            final Composite parent) {
        super();
        init(name, labelText);
        createControl(parent);
    }

    /**
     * {@inheritDoc}.
     * 
     * (non-Javadoc)
     * 
     * @seeorg.eclipse.ui.internal.ide.dialogs.AbstractEncodingFieldEditor#
     *                                                                      getStoredValue
     *                                                                      ()
     */
    protected String getStoredValue() {
        return getPreferenceStore().getString(getPreferenceName());
    }

    /**
     * {@inheritDoc}.
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.FieldEditor#doStore()
     */
    protected void doStore() {
        String encoding = getSelectedEncoding();

        if (hasSameEncoding(encoding)) {
            return;
        }

        IDEEncoding.addIDEEncoding(encoding);

        if (encoding.equals(getDefaultEnc())) {
            getPreferenceStore().setToDefault(getPreferenceName());
        } else {
            getPreferenceStore().setValue(getPreferenceName(), encoding);
        }
    }

    /**
     * {@inheritDoc}.
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.ide.dialogs.AbstractEncodingFieldEditor#setPreferenceStore
     *      (org.eclipse.jface.preference.IPreferenceStore)
     */
    @Override
    public void setPreferenceStore(final IPreferenceStore store) {
        if (store != null) {
            super.setPreferenceStore(store);
        }
    }

}

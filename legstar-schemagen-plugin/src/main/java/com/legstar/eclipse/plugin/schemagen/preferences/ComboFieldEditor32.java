package com.legstar.eclipse.plugin.schemagen.preferences;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/**
 * This is a clone of Eclipse ComboFieldEditor that appeared in Eclipse 3.3.
 * Since we are compatible with Eclipse 3.2, we can't assume this calss will be
 * present.
 * 
 */
public class ComboFieldEditor32 extends FieldEditor {

    /**
     * The <code>Combo</code> widget.
     */
    private Combo fCombo;

    /**
     * The value (not the name) of the currently selected item in the Combo
     * widget.
     */
    private String fValue;

    /**
     * The names (labels) and underlying values to populate the combo widget.
     * These should be
     * arranged as: { {name1, value1}, {name2, value2}, ...}
     */
    private String[][] fEntryNamesAndValues;

    /**
     * Create the combo box field editor.
     * 
     * @param name the name of the preference this field editor works on
     * @param labelText the label text of the field editor
     * @param entryNamesAndValues the names (labels) and underlying values to
     *            populate the combo widget. These should be
     *            arranged as: { {name1, value1}, {name2, value2}, ...}
     * @param parent the parent composite
     */
    public ComboFieldEditor32(
            final String name, final String labelText,
            final String[][] entryNamesAndValues, final Composite parent) {
        init(name, labelText);
        Assert.isTrue(checkArray(entryNamesAndValues));
        fEntryNamesAndValues = entryNamesAndValues;
        createControl(parent);
    }

    /**
     * Checks whether given <code>String[][]</code> is of "type"
     * <code>String[][2]</code>.
     * 
     * @param table the array
     * @return <code>true</code> if it is ok, and <code>false</code> otherwise
     */
    private boolean checkArray(final String[][] table) {
        if (table == null) {
            return false;
        }
        for (int i = 0; i < table.length; i++) {
            String[] array = table[i];
            if (array == null || array.length != 2) {
                return false;
            }
        }
        return true;
    }

    /**
     * {@inheritDoc}.
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.FieldEditor#adjustForNumColumns(int)
     */
    protected void adjustForNumColumns(final int numColumns) {
        if (numColumns > 1) {
            Control control = getLabelControl();
            int left = numColumns;
            if (control != null) {
                ((GridData) control.getLayoutData()).horizontalSpan = 1;
                left = left - 1;
            }
            ((GridData) fCombo.getLayoutData()).horizontalSpan = left;
        } else {
            Control control = getLabelControl();
            if (control != null) {
                ((GridData) control.getLayoutData()).horizontalSpan = 1;
            }
            ((GridData) fCombo.getLayoutData()).horizontalSpan = 1;
        }
    }

    /**
     * {@inheritDoc}.
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.FieldEditor#doFillIntoGrid(org.eclipse.swt
     *      .widgets.Composite, int)
     */
    protected void doFillIntoGrid(final Composite parent, final int numColumns) {
        int comboC = 1;
        if (numColumns > 1) {
            comboC = numColumns - 1;
        }
        Control control = getLabelControl(parent);
        GridData gd = new GridData();
        gd.horizontalSpan = 1;
        control.setLayoutData(gd);
        control = getComboBoxControl(parent);
        gd = new GridData();
        gd.horizontalSpan = comboC;
        gd.horizontalAlignment = GridData.FILL;
        control.setLayoutData(gd);
        control.setFont(parent.getFont());
    }

    /**
     * {@inheritDoc}.
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.FieldEditor#doLoad()
     */
    protected void doLoad() {
        updateComboForValue(getPreferenceStore().getString(getPreferenceName()));
    }

    /**
     * {@inheritDoc}.
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.FieldEditor#doLoadDefault()
     */
    protected void doLoadDefault() {
        updateComboForValue(getPreferenceStore().getDefaultString(
                getPreferenceName()));
    }

    /**
     * {@inheritDoc}.
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.FieldEditor#doStore()
     */
    protected void doStore() {
        if (fValue == null) {
            getPreferenceStore().setToDefault(getPreferenceName());
            return;
        }
        getPreferenceStore().setValue(getPreferenceName(), fValue);
    }

    /**
     * {@inheritDoc}.
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.FieldEditor#getNumberOfControls()
     */
    public int getNumberOfControls() {
        return 2;
    }

    /**
     * Lazily create and return the Combo control.
     * 
     * @param parent the parent composite
     * @return the combo box
     */
    private Combo getComboBoxControl(final Composite parent) {
        if (fCombo == null) {
            fCombo = new Combo(parent, SWT.READ_ONLY);
            fCombo.setFont(parent.getFont());
            for (int i = 0; i < fEntryNamesAndValues.length; i++) {
                fCombo.add(fEntryNamesAndValues[i][0], i);
            }

            fCombo.addSelectionListener(new SelectionAdapter() {
                public void widgetSelected(final SelectionEvent evt) {
                    String oldValue = fValue;
                    String name = fCombo.getText();
                    fValue = getValueForName(name);
                    setPresentsDefaultValue(false);
                    fireValueChanged(VALUE, oldValue, fValue);
                }
            });
        }
        return fCombo;
    }

    /**
     * Given the name (label) of an entry, return the corresponding value.
     * 
     * @param name the name
     * @return the value
     */
    private String getValueForName(final String name) {
        for (int i = 0; i < fEntryNamesAndValues.length; i++) {
            String[] entry = fEntryNamesAndValues[i];
            if (name.equals(entry[0])) {
                return entry[1];
            }
        }
        return fEntryNamesAndValues[0][0];
    }

    /**
     * Set the name in the combo widget to match the specified value.
     * 
     * @param value the value
     */
    private void updateComboForValue(final String value) {
        fValue = value;
        for (int i = 0; i < fEntryNamesAndValues.length; i++) {
            if (value.equals(fEntryNamesAndValues[i][1])) {
                fCombo.setText(fEntryNamesAndValues[i][0]);
                return;
            }
        }
        if (fEntryNamesAndValues.length > 0) {
            fValue = fEntryNamesAndValues[0][1];
            fCombo.setText(fEntryNamesAndValues[0][0]);
        }
    }

    /**
     * {@inheritDoc}.
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.preference.FieldEditor#setEnabled(boolean,
     *      org.eclipse.swt.widgets.Composite)
     */
    public void setEnabled(final boolean enabled, final Composite parent) {
        super.setEnabled(enabled, parent);
        getComboBoxControl(parent).setEnabled(enabled);
    }
}

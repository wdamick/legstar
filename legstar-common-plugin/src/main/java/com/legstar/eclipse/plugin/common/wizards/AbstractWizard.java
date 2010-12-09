/*******************************************************************************
 * Copyright (c) 2010 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.eclipse.plugin.common.wizards;

import java.lang.reflect.InvocationTargetException;
import java.util.Properties;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.widgets.Shell;
import org.osgi.service.prefs.BackingStoreException;

import com.legstar.eclipse.plugin.common.Activator;
import com.legstar.eclipse.plugin.common.Messages;

/**
 * A generic wizard gathering shared code used by derived wizards.
 * 
 */
public abstract class AbstractWizard extends Wizard {

    /** Unlocks the finish button. */
    private boolean _canFinish = false;

    /** Set of preferences stored at the project level. */
    private IEclipsePreferences _projectPreferences;

    /**
     * Pops an error message.
     * 
     * @param shell parent shell
     * @param dialogTitle the error dialog title
     * @param pluginID the parent plugin ID
     * @param shortMessage text of the message
     * @param reason additional explanations
     */
    public static void errorDialog(
            final Shell shell,
            final String dialogTitle,
            final String pluginID,
            final String shortMessage,
            final String reason) {
        IStatus status = new Status(
                IStatus.ERROR, pluginID,
                IStatus.ERROR, reason, null);
        ErrorDialog.openError(shell, dialogTitle, shortMessage, status);
    }

    /**
     * Logs an exception using IStatus.
     * 
     * @param innerException the exception to trace
     * @param pluginID plug-in identifier
     */
    public static void logCoreException(
            final Throwable innerException,
            final String pluginID) {
        Activator.logCoreException(innerException, pluginID);
    }

    /**
     * Create a formatted core exception.
     * 
     * @param e an exception
     * @throws CoreException the core exception
     */
    public static void throwCoreException(
            final Exception e) throws CoreException {
        Activator.throwCoreException(e);
    }

    /**
     * Create a formatted core exception.
     * 
     * @param message the error message
     * @throws CoreException the core exception
     */
    public static void throwCoreException(
            final String message) throws CoreException {
        Activator.throwCoreException(message);
    }

    /**
     * This method is called when 'Finish' button is pressed in the wizard.
     * We will create an operation and run it using wizard as execution context.
     * 
     * @return true if processing went fine
     */
    public boolean performFinish() {

        try {
            storeProjectPreferences();
            IRunnableWithProgress op = getWizardRunnable();
            if (op != null) {
                getContainer().run(true, true, op);
            }
        } catch (InterruptedException e) {
            return false;
        } catch (InvocationTargetException e) {
            signalGenerationError(e.getTargetException());
            return false;
        } catch (BackingStoreException e) {
            signalGenerationError(e);
            return false;
        }
        return true;
    }

    /**
     * Make sure user is aware that something went wrong.
     * 
     * @param e the generation exception
     */
    protected void signalGenerationError(final Throwable e) {
        errorDialog(getShell(),
                NLS.bind(Messages.generate_error_dialog_title,
                        getGenerationSubject()),
                getPluginId(),
                NLS.bind(Messages.generation_failure_short_msg,
                        getGenerationSubject()),
                NLS.bind(Messages.generation_failure_long_msg,
                        getGenerationSubject(), e));
        logCoreException(e, getPluginId());
    }

    /**
     * Saves the user choices reflected by the model in a preference node.
     * 
     * @throws BackingStoreException if access to properties store fails
     */
    protected void storeProjectPreferences() throws BackingStoreException {
        Properties props = getPersistProperties();
        if (props != null) {
            storeProperties(getProjectPreferences(), getPersistProperties());
            try {
                getProjectPreferences().flush();
            } catch (BackingStoreException e) {
                logCoreException(e, getPluginId());
            }
        }
    }

    /**
     * @return a runnable job to be executed on finish
     * 
     * @throws InvocationTargetException if runnable cannot be created
     */
    public abstract IRunnableWithProgress getWizardRunnable()
            throws InvocationTargetException;

    /**
     * Determines where the common LegStar plugin is installed on the file
     * system.
     * 
     * @param pluginId the plugin identifier for which the location is sought
     * @return the plugin location
     * @throws InvocationTargetException if location cannot be determined
     */
    public static String getPluginInstallLocation(final String pluginId)
            throws InvocationTargetException {
        return Activator.getPluginInstallLocation(pluginId);
    }

    /**
     * @return the model fields that need to be persisted
     */
    public abstract Properties getPersistProperties();

    /**
     * @return the current plugin ID
     */
    public abstract String getPluginId();

    /**
     * @return a label to describe what is generated by this wizard
     */
    public abstract String getGenerationSubject();

    /**
     * {@inheritDoc}
     * 
     * @see org.eclipse.jface.wizard.Wizard#canFinish()
     */
    public boolean canFinish() {
        return _canFinish;
    }

    /**
     * @param canFinish true if finish button should be available
     */
    public void setCanFinish(final boolean canFinish) {
        _canFinish = canFinish;
    }

    /**
     * Store properties in a set of preferences removing any previous
     * preferences.
     * <p/>
     * We assume all values and keys are strings and are not null. This also
     * assumes that the store can be cleared prior to storing the new properties
     * set.
     * 
     * @param preferences the Eclipse preference store
     * @param props the properties file
     * @throws BackingStoreException if access to preference store fails
     */
    public void storeProperties(final IEclipsePreferences preferences,
            final Properties props) throws BackingStoreException {

        preferences.clear();
        for (String key : props.stringPropertyNames()) {
            String value = props.getProperty(key);
            preferences.put(key, value);
        }
    }

    /**
     * Create a properties file from preferences content.
     * 
     * @param preferences an Eclipse preference store
     * @return a properties file
     * @throws BackingStoreException if access to preference store fails
     */
    public Properties loadProperties(final IEclipsePreferences preferences)
            throws BackingStoreException {
        Properties props = new Properties();
        for (String key : preferences.keys()) {
            props.put(key, preferences.get(key, null));
        }
        return props;
    }

    /**
     * Sets a project preference store.
     * 
     * @param project the Eclipse project
     */
    public void setProjectPreferences(final IProject project) {
        IScopeContext context = new ProjectScope(project);
        _projectPreferences = context.getNode(getPluginId());
    }

    /**
     * @return the project scope preferences or the default preferences
     *         if none is set at the project level
     */
    public IEclipsePreferences getProjectPreferences() {
        return _projectPreferences;
    }
}

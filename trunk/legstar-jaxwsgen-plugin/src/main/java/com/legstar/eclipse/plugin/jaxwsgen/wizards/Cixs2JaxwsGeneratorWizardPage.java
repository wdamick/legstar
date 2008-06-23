package com.legstar.eclipse.plugin.jaxwsgen.wizards;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;

import com.legstar.eclipse.plugin.cixscom.wizards.AbstractCixsActivator;
import com.legstar.eclipse.plugin.cixscom.wizards
		.AbstractCixsGeneratorWizardPage;
import com.legstar.eclipse.plugin.common.wizards.IURLSelectionListener;
import com.legstar.eclipse.plugin.jaxwsgen.Activator;
import com.legstar.eclipse.plugin.jaxwsgen.Messages;
import com.legstar.eclipse.plugin.jaxwsgen.dialogs.WsdlPortSelectionDialog;
import com.legstar.eclipse.plugin.jaxwsgen.preferences.PreferenceConstants;

/**
 * A wizard page displaying widgets that are specific to the Cixs to Jaxws
 * generation.
 */
public class Cixs2JaxwsGeneratorWizardPage
        extends AbstractCixsGeneratorWizardPage {

    /** Page name. */
    private static final String PAGE_NAME = "Cixs2JaxwsGeneratorWizardPage";
    
    /** URL locating target Web service WSDL. */
    private Combo mWsdlUrlCombo = null;
    
    /** Target Web service WSDL service name. */
    private Text mWsdlServiceNameText = null;
    
    /** Target Web service WSDL port name. */
    private Text mWsdlPortNameText = null;
    
    /** Target Web services target namespace. */
    private Text mTargetNamespaceText = null;
    
    /** Where generated COBOL source reside. */
    private Text mTargetCobolDirText = null;

    /** J2ee folder where web deployment files should be generated. */
    private Text mTargetWDDDirText = null;
    
    /** J2ee folder where war files should be deployed. */
    private Text mTargetWarDirText = null;
    
    /** Proxy service URI exposed to mainframe programs. */
    private Text mProxyURIText = null;

    /** Proxy service user ID. */
    private Text mProxyUserIdText = null;

    /** Proxy service URI exposed to mainframe programs. */
    private Text mProxyPasswordText = null;
    
    /**
     * Construct the page.
     * @param selection the current workbench selection
     * @param mappingFile the mapping file
     */
    protected Cixs2JaxwsGeneratorWizardPage(
            final IStructuredSelection selection,
            final IFile mappingFile) {
        super(selection, PAGE_NAME,
        		Messages.cixs_to_jaxws_wizard_page_title,
        		Messages.cixs_to_jaxws_wizard_page_description,
        		mappingFile);
    }

    /** {@inheritDoc} */
    protected void addCixsGroup(final Composite container) {
        Group group = createGroup(container, Messages.wsdl_group_label, 3);
        
        mWsdlUrlCombo = createUrlComboGroup(
        		group, Messages.wsdl_url_label,
        		new ModifyListener() {
                    public void modifyText(final ModifyEvent e) {
                        dialogChanged();
                    }
                },
        		new URLSelectionAdapter());
        
        mWsdlServiceNameText = createTextField(group, getStore(),
                "wsdlServiceName", Messages.wsdl_service_name_label + ':');
        mWsdlServiceNameText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        createLabel(group, "");

        mWsdlPortNameText = createTextField(group, getStore(),
                "wsdlPorteName", Messages.wsdl_port_name_label + ':');
        mWsdlPortNameText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        createLabel(group, "");

        mTargetNamespaceText = createTextField(group, getStore(),
                "targetNamespace", Messages.wsdl_target_namespace_label + ':');
    	mTargetNamespaceText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        createLabel(group, "");
        
        super.addCixsGroup(container);
    }
    
	/**
	 *Defines what happens when a URL is selected.
	 */
	private class URLSelectionAdapter implements IURLSelectionListener {
	
		/** {@inheritDoc} */
		public void urlSelected(final String urlString) {
        	WsdlPortSelectionDialog dlg =
                new WsdlPortSelectionDialog(
                		getWsdlUrl(), getShell(),
                		Activator.PLUGIN_ID);
            if (Window.OK == dlg.open()) {
            	setTargetNamespace(dlg.getTargetNamespace());
            	setWsdlServiceName(dlg.getServiceName());
            	setWsdlPortName(dlg.getPortName());
            }
		}
	}
	
    /** {@inheritDoc} */
    public void addWidgetsToCixsGroup(final Composite container) {
    }

    /** {@inheritDoc} */
    public void addWidgetsToTargetGroup(final Composite container) {
        mTargetWDDDirText = createDirectoryFieldEditor(container,
                "targetWDDDir", Messages.wdd_target_location_label + ':');
        mTargetWDDDirText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        mTargetCobolDirText = createDirectoryFieldEditor(container,
                "targetCobolDir", Messages.cobol_target_location_label + ':');
        mTargetCobolDirText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
    }

    /** {@inheritDoc} */
    public void addWidgetsToCoxbGroup(final Composite container) {
    }

    /** {@inheritDoc} */
    public void addWidgetsToDeploymentGroup(final Composite container) {
        createLabel(container, Messages.proxy_uri_label + ':');
        mProxyURIText = createText(container); 
        mProxyURIText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        createLabel(container, Messages.proxy_user_id_label + ':');
        mProxyUserIdText = createText(container); 
        mProxyUserIdText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        createLabel(container, Messages.proxy_password_label + ':');
        mProxyPasswordText = createText(container); 
        mProxyPasswordText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
        mTargetWarDirText = createTextField(container, getStore(),
                "targetJarDir", Messages.war_deployment_location_label + ':');
        mTargetWarDirText.addModifyListener(new ModifyListener() {
            public void modifyText(final ModifyEvent e) {
                dialogChanged();
            }
        });
    }

    /** {@inheritDoc} */
    public void initExtendedWidgets(final IProject project) {
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        initWsdlUrl();
        
        setTargetWDDDir(getDefaultTargetDir(store,
				PreferenceConstants.J2EE_WDD_FOLDER));
        
        setTargetCobolDir(getDefaultTargetDir(store,
				PreferenceConstants.COBOL_SAMPLE_FOLDER));
        
        setTargetWarDir(store.getDefaultString(
				PreferenceConstants.J2EE_WAR_FOLDER));
        
        initServiceURI(getServiceName());
    }
    
    /**
     * Setup the initial history list attached to wsdl URL combo box.
     */
    private void initWsdlUrl() {
        for (String value : getUrlHistory().get()) {
        	mWsdlUrlCombo.add(value);
        }
    }

	/**
	 * The default service URI is built from a template. 
	 * @param serviceName the service name
	 */
	private void initServiceURI(final String serviceName) {
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		String template = store.getString(
				PreferenceConstants.PROXY_URI_TEMPLATE);
		if (template != null && template.length() > 0) {
			setProxyURI(template.replace("${service.name}", serviceName));
		}
	}

	/** {@inheritDoc} */
    public boolean validateExtendedWidgets() {
        if (getWsdlUrl().length() == 0) {
			updateStatus(Messages.invalid_wsdl_url_msg);
			return false;
        }
        if (getWsdlServiceName().length() == 0) {
			updateStatus(Messages.invalid_wsdl_service_name_msg);
			return false;
        }
        if (getWsdlPortName().length() == 0) {
			updateStatus(Messages.invalid_wsdl_port_name_msg);
			return false;
        }
        if (!checkDirectory(getTargetWDDDir(),
        		Messages.invalid_wdd_target_location_msg)) {
        	return false;
        }
        if (!checkDirectory(getTargetCobolDir(),
        		Messages.invalid_cobol_target_location_msg)) {
        	return false;
        }
        if (getTargetNamespace().length() == 0) {
			updateStatus(Messages.invalid_target_namespace_msg);
			return false;
        }
        if (getProxyURI().length() == 0) {
        	if (getServiceName().length() > 0) {
        		initServiceURI(getServiceName());
        	}
        }

        return true;
    }

    /**
     * @param targetWDDDir J2ee folder where web deployment files should
     *  be generated
     */
    public void setTargetWDDDir(final String targetWDDDir) {
        mTargetWDDDirText.setText(targetWDDDir);
    }
    
    /**
     * @return J2ee folder where web deployment files should be generated
     */
    public String getTargetWDDDir() {
        return mTargetWDDDirText.getText();
    }

    /**
     * @param targetNamespace Generated Web services target namespace
     */
    public void setTargetNamespace(final String targetNamespace) {
        mTargetNamespaceText.setText(targetNamespace);
    }
    
    /**
     * @return Generated Web services target namespace
     */
    public String getTargetNamespace() {
        return mTargetNamespaceText.getText();
    }

    /** {@inheritDoc} */
    public AbstractCixsActivator getActivator() {
        return Activator.getDefault();
    }

	/**
	 * @return the URL locating target Web service WSDL
	 */
	public final String getWsdlUrl() {
		return mWsdlUrlCombo.getText();
	}

	/**
	 * @param wsdlUrl the URL locating target Web service WSDL to set
	 */
	public final void setWsdlUrl(final String wsdlUrl) {
		mWsdlUrlCombo.setText(wsdlUrl);
	}

	/**
	 * @return the Target Web service WSDL service name
	 */
	public final String getWsdlServiceName() {
		return mWsdlServiceNameText.getText();
	}

	/**
	 * @param wsdlServiceName the Target Web service WSDL service name to
	 *  set
	 */
	public final void setWsdlServiceName(final String wsdlServiceName) {
		mWsdlServiceNameText.setText(wsdlServiceName);
	}

	/**
	 * @return the Target Web service WSDL port name
	 */
	public final String getWsdlPortName() {
		return mWsdlPortNameText.getText();
	}

	/**
	 * @param wsdlPortName the Target Web service WSDL port name to set
	 */
	public final void setWsdlPortName(final String wsdlPortName) {
		mWsdlPortNameText.setText(wsdlPortName);
	}

	/**
	 * @return where generated COBOL source reside
	 */
	public final String getTargetCobolDir() {
		return mTargetCobolDirText.getText();
	}

	/**
	 * @param targetCobolDir where generated COBOL source reside to set
	 */
	public final void setTargetCobolDir(final String targetCobolDir) {
		mTargetCobolDirText.setText(targetCobolDir);
	}

	/**
	 * @return the J2ee folder where war files should be deployed
	 */
	public final String getTargetWarDir() {
		return mTargetWarDirText.getText();
	}

	/**
	 * @param targetWarDir J2ee folder where war files should be deployed
	 */
	public final void setTargetWarDir(final String targetWarDir) {
		mTargetWarDirText.setText(targetWarDir);
	}

	/**
	 * @return the Proxy service URI exposed to mainframe programs
	 */
	public final String getProxyURI() {
		return mProxyURIText.getText();
	}

	/**
	 * @param proxyURI the Proxy service URI exposed to mainframe programs
	 *  to set
	 */
	public final void setProxyURI(final String proxyURI) {
		mProxyURIText.setText(proxyURI);
	}

	/**
	 * @return the Proxy service user ID
	 */
	public final String getProxyUserId() {
		return mProxyUserIdText.getText();
	}

	/**
	 * @param proxyUserId the Proxy service user ID to set
	 */
	public final void setProxyUserId(final String proxyUserId) {
		mProxyUserIdText.setText(proxyUserId);
	}

	/**
	 * @return the Proxy service URI exposed to mainframe programs
	 */
	public final String getProxyPassword() {
		return mProxyPasswordText.getText();
	}

	/**
	 * @param proxyPassword the Proxy service URI exposed to mainframe
	 *  programs to set
	 */
	public final void setProxyPassword(final String proxyPassword) {
		mProxyPasswordText.setText(proxyPassword);
	}


}

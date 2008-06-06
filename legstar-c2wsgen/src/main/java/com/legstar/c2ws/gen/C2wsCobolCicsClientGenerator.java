package com.legstar.c2ws.gen;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.codegen.CodeGenHelper;
import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.CodeGenUtil;

/**
 * This ant generator creates a COBOL CICS program to act as a sample to
 * execute a remote service over HTTP.
 *
 */
public class C2wsCobolCicsClientGenerator extends Task {

	/** This generator name. */
    public static final String C2WS_COBOL_CICS_CLIENT_GENERATOR_NAME =
        "LegStar COBOL CICS Client generator";

    /** Velocity template for COBOL CICS client. */
    public static final String C2WS_COBOL_CICS_CLIENT_VLC_TEMPLATE =
        "vlc/c2ws-cobol-cics-client.vm";
    
    /** The generation model groups all parameters needed by templates. */
    private C2wsOperationModel mModel;

    /** The target directory where COBOL files will be created. */
    private File mTargetCobolDir;
    
    /** Logger. */
    private static final Log LOG = LogFactory.getLog(C2wsCobolCicsClientGenerator.class);

    /** @{inheritDoc}*/
    @Override
    public final void init() {
        LOG.info("Initializing velocity engine for "
                + C2WS_COBOL_CICS_CLIENT_GENERATOR_NAME);
        try {
            CodeGenUtil.initVelocity();
            mModel = new C2wsOperationModel();
        } catch (Exception e) {
            throw new BuildException(e.getMessage());
        }
    }
    
    /**
     * Check that input values are valid.
     * @throws CodeGenMakeException if input is invalid
     */
    public void checkInput() throws CodeGenMakeException {
        if (getCixsOperation() == null) {
            throw new CodeGenMakeException(
                    "Missing cixs operation parameter");
        }
        try {
            CodeGenUtil.checkDirectory(getTargetCobolDir(), true);
            if (getServiceURI() == null || getServiceURI().length() == 0) {
                throw new CodeGenMakeException(
                	"You must specify a service URI");
            }
            URI uri = new URI(getServiceURI());
            if (uri.getScheme() == null ||
            		uri.getScheme().compareToIgnoreCase("http") != 0) {
                throw new CodeGenMakeException(
                		"URI " + uri + " must have http scheme");
            }
            if (getServiceName() == null || getServiceName().length() == 0) {
                throw new CodeGenMakeException(
                        "You must provide a service name");
            }
            String cicsProgramName = getCixsOperation().getCicsProgramName();
            if (cicsProgramName == null || cicsProgramName.length() == 0) {
                throw new CodeGenMakeException(
                        "Operation must specify a CICS program name");
            }
        } catch (IllegalArgumentException e) {
            throw new CodeGenMakeException(e);
        } catch (URISyntaxException e) {
            throw new CodeGenMakeException(e);
		}
    }

    /**
     * Check that enough input parameters are set and then
     * generate the requested artifacts.
     * 
     * */
    @Override
    public final void execute() {
        LOG.info("Generating COBOL CICS Client code");
        long start = System.currentTimeMillis();

        try {
            checkInput();
            generate();

        } catch (CodeGenMakeException e) {
            LOG.error(C2WS_COBOL_CICS_CLIENT_GENERATOR_NAME + " failure", e);
            throw new BuildException(e);
        }

        long end = System.currentTimeMillis();
        LOG.info("Generation success for " + getCixsOperation().getName());
        LOG.info("Duration = " + (end - start) + " ms");
    }

    /**
     * Create artifacts.
     * @throws CodeGenMakeException if generation fails
     */
    public void generate() throws CodeGenMakeException {
        Map < String, Object > parameters = new HashMap < String, Object >();
        CodeGenHelper helper = new CodeGenHelper();
        parameters.put("helper", helper);
        
        generateCobolCicsClient(mModel, parameters, getTargetCobolDir());
    }
    
    /**
     * Create the COBOL CICS Program file.
     * @param model the operation description
     * @param parameters miscellaneous help parameters
     * @param cobolFilesDir where to store the generated file
     * @throws CodeGenMakeException if generation fails
     */
    public static void generateCobolCicsClient(
            final C2wsOperationModel model,
            final Map < String, Object > parameters,
            final File cobolFilesDir)
    throws CodeGenMakeException {

        File targetFile = CodeGenUtil.getFile(cobolFilesDir,
        		model.getCicsProgramName() + ".cbl");
        LOG.info("Generating " + targetFile.getAbsolutePath());
        CodeGenUtil.processTemplate(
        		C2WS_COBOL_CICS_CLIENT_GENERATOR_NAME,
        		C2WS_COBOL_CICS_CLIENT_VLC_TEMPLATE,
                "model", model,
                parameters,
                targetFile);
    }

	/**
	 * @return the URI that the host must use to reach the remote service
	 */
	public final String getServiceURI() {
		return mModel.getServiceURI();
	}

	/**
	 * @param serviceURI the URI that the host must use to reach the remote
	 *  service to set
	 */
	public final void setServiceURI(final String serviceURI) {
		mModel.setServiceURI(serviceURI);
	}

	/**
	 * @return the User ID to present remote service
	 */
	public final String getServiceUserId() {
		return mModel.getServiceUserId();
	}

	/**
	 * @param serviceUserId the User ID to present remote service to set
	 */
	public final void setServiceUserId(final String serviceUserId) {
		mModel.setServiceUserId(serviceUserId);
	}

	/**
	 * @return the Password to present remote service
	 */
	public final String getServicePassword() {
		return mModel.getServicePassword();
	}

	/**
	 * @param servicePassword the Password to present remote service to set
	 */
	public final void setServicePassword(final String servicePassword) {
		mModel.setServicePassword(servicePassword);
	}

	/**
	 * @return the Name of the remote service
	 */
	public final String getServiceName() {
		return mModel.getServiceName();
	}

	/**
	 * @param serviceName the Name of the remote service to set
	 */
	public final void setServiceName(final String serviceName) {
		mModel.setServiceName(serviceName);
	}

	/**
	 * @return the Operation
	 */
	public final CixsOperation getCixsOperation() {
		return mModel.getCixsOperation();
	}

	/**
	 * @param cixsOperation the Operation to set
	 */
	public final void setCixsOperation(final CixsOperation cixsOperation) {
		mModel.setCixsOperation(cixsOperation);
	}

	/**
	 * @param cixsOperation the Operation to set
	 */
	public final void add(final CixsOperation cixsOperation) {
		setCixsOperation(cixsOperation);
	}

	/**
	 * @param cixsOperation the Operation to set
	 */
	public final void addCixsOperation(
			final CixsOperation cixsOperation) {
		setCixsOperation(cixsOperation);
	}

	/**
	 * @return the directory where COBOL files will be created
	 */
	public final File getTargetCobolDir() {
		return mTargetCobolDir;
	}

	/**
	 * @param targetCobolDir the directory where COBOL files will be created to set
	 */
	public final void setTargetCobolDir(File targetCobolDir) {
		mTargetCobolDir = targetCobolDir;
	}

}

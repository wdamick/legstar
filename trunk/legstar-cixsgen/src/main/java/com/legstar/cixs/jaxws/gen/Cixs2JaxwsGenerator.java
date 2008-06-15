package com.legstar.cixs.jaxws.gen;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Map;

import com.legstar.cixs.gen.ant.AbstractCixsGenerator;
import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.jaxws.model.AntBuildCixs2JaxwsModel;
import com.legstar.cixs.jaxws.model.CixsJaxwsService;
import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.CodeGenUtil;

/**
 * This Ant task creates the various Jaxws artifacts needed to implement
 * a servlet proxy that acts as an adapter for a Web Service so that a
 * mainframe program can call the target Web Service without any knowledge
 * of SOAP.
 * The task also generates a sample COBOL CICS program that demonstrates
 * how to call the proxy servlet. 
 */
public class Cixs2JaxwsGenerator extends AbstractCixsGenerator {

	/** This generator name. */
	private static final String JAXWS_CIXS_GENERATOR_NAME =
		"LegStar Web Service proxy for Mainframe generator";

	/** Velocity template for war ant build generation. */
	public static final String SERVICE_ANT_BUILD_WAR_VLC_TEMPLATE =
		"vlc/c2j-service-ant-build-war-xml.vm";

	/** Velocity template for web descriptor generation. */
	public static final String SERVICE_WEB_XML_VLC_TEMPLATE =
		"vlc/c2j-service-web-xml.vm";

	/** Velocity template for COBOL client generation. */
	public static final String OPERATION_COBOL_CICS_CLIENT_VLC_TEMPLATE =
		"vlc/c2j-operation-cobol-cics-client.vm";

	/**
     * Constructor.
     */
	public Cixs2JaxwsGenerator() {
		super(new AntBuildCixs2JaxwsModel());
	}

	/** {@inheritDoc}*/
	public void checkExtendedInput() throws CodeGenMakeException {
        try {
			CodeGenUtil.checkDirectory(getTargetWDDDir(), true);
			CodeGenUtil.checkDirectory(getTargetCobolDir(), true);
			String serviceURI = getCixsJaxwsService().getServiceURI();
            if (serviceURI == null || serviceURI.length() == 0) {
                throw new CodeGenMakeException(
                	"You must specify a service URI");
            }
            URI uri = new URI(serviceURI);
            if (uri.getScheme() == null
            		|| uri.getScheme().compareToIgnoreCase("http") != 0) {
                throw new CodeGenMakeException(
                		"URI " + uri + " must have http scheme");
            }
            for (CixsOperation operation : getCixsOperations()) {
                String cicsProgramName = operation.getCicsProgramName();
                if (cicsProgramName == null || cicsProgramName.length() == 0) {
                    throw new CodeGenMakeException(
                            "Operation must specify a CICS program name");
                }
            }
        } catch (IllegalArgumentException e) {
            throw new CodeGenMakeException(e);
        } catch (URISyntaxException e) {
            throw new CodeGenMakeException(e);
		}
	}

	/** {@inheritDoc}*/
	public void generate(final Map < String, Object > parameters)
			throws CodeGenMakeException {
		
        parameters.put("targetWarDir", getTargetWarDir());
        parameters.put("targetWDDDir", getTargetWDDDir());
        parameters.put("hostCharset", getHostCharset());
		
        /* TODO fill this to generate ant build */
	}

	/**
	 * @return the Target location for web deployment descriptors
	 */
	public final File getTargetWDDDir() {
		return getModel().getTargetWDDDir();
	}

	/**
	 * @param targetWDDDir the Target location for web deployment descriptors to
	 *  set
	 */
	public final void setTargetWDDDir(final File targetWDDDir) {
		getModel().setTargetWDDDir(targetWDDDir);
	}

	/**
	 * @return the deployment location for jaxws war files
	 */
	public final File getTargetWarDir() {
		return getModel().getTargetWarDir();
	}

	/**
	 * @param targetWarDir the deployment location for jaxws war files to set
	 */
	public final void setTargetWarDir(final File targetWarDir) {
		getModel().setTargetWarDir(targetWarDir);
	}
	
	/**
	 * @return the directory where COBOL files will be created
	 */
	public final File getTargetCobolDir() {
		return getModel().getTargetCobolDir();
	}

	/**
	 * @param targetCobolDir the directory where COBOL files will be created
	 *  to set
	 */
	public final void setTargetCobolDir(final File targetCobolDir) {
		getModel().setTargetCobolDir(targetCobolDir);
	}

	/**
	 * @return the service description
	 */
	public final CixsJaxwsService getCixsJaxwsService() {
		return (CixsJaxwsService) getCixsService();
	}
	
	/**
	 * @param cixsJaxwsService the service description to set
	 */
	public final void setCixsJaxwsService(
			final CixsJaxwsService cixsJaxwsService) {
		setCixsService(cixsJaxwsService);
	}

	/**
	 * @param cixsJaxwsService the Jaxws service to set
	 */
	public final void add(final CixsJaxwsService cixsJaxwsService) {
		setCixsService(cixsJaxwsService);
	}

	/**
	 * @param cixsJaxwsService the Jaxws service to set
	 */
	public final void addCixsJaxwsService(
			final CixsJaxwsService cixsJaxwsService) {
		setCixsService(cixsJaxwsService);
	}

   /**
     * {@inheritDoc}
     * @see com.legstar.cixs.gen.ant.AbstractCixsGenerator#getModel()
     */
    public AntBuildCixs2JaxwsModel getModel() {
        return (AntBuildCixs2JaxwsModel) super.getModel();
    }
    
    /**
     * Convenience method to get the inner mapped operations.
     * @return the operations list
     */
    public List < CixsOperation > getCixsOperations() {
    	return getCixsJaxwsService().getCixsOperations();
    }
    
	/** {@inheritDoc}*/
	public String getGeneratorName() {
		return JAXWS_CIXS_GENERATOR_NAME;
	}

}

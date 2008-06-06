package com.legstar.c2ws.gen;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.util.List;

import com.legstar.cixs.gen.model.CixsOperation;
import com.legstar.cixs.gen.model.CixsStructure;
import com.legstar.cobc.gen.CobolGenVisitor;
import com.legstar.codegen.CodeGenMakeException;
import com.legstar.codegen.models.AbstractAntBuildModel;
import com.legstar.coxb.host.HostException;
import com.legstar.coxb.impl.reflect.CComplexReflectBinding;
import com.legstar.coxb.impl.reflect.ReflectBindingException;
import com.legstar.xsdc.gen.CobolNameResolver;
import com.legstar.xsdc.gen.CobolNameResolverException;

/**
 * This model groups parameters needed to generate artifacts for an inbound
 * CICS operation. One such artifacts is a CICS COBOL source that showcases
 * how a remote service can called from CICS.
 */
public class C2wsOperationModel extends AbstractAntBuildModel {

	/** The operation being made available to CICS. */
	private CixsOperation mCixsOperation;

	/** The URI that the host must use to reach the remote service. */
	private String mServiceURI;

	/** User ID to present remote service. */
	private String mServiceUserId;

	/** Password to present remote service. */
	private String mServicePassword;

	/** Name of the remote service. */
	private String mServiceName;

	private enum Direction { INPUT, OUTPUT };

    /** Helper to suggest COBOL names from Java names. */
    private CobolNameResolver mCobolNameResolver;
    
    /**
     * Construct an empty model.
 	 * @throws CobolNameResolverException if name resolver cannot be instantiated
    */
    public C2wsOperationModel() throws CobolNameResolverException {
        mCobolNameResolver = new CobolNameResolver();
    }

	/**
	 * Construct a model from an operation description.
	 * @param cixsOperation the operation
	 * @throws CobolNameResolverException if name resolver cannot be instantiated
	 */
	public C2wsOperationModel(
			final CixsOperation cixsOperation) throws CobolNameResolverException {
		this();
		mCixsOperation = cixsOperation;
	}

	public void generateBuild(File scriptFile) throws CodeGenMakeException {
		// TODO Auto-generated method stub

	}

	public String getCicsProgramName() {
		return mCixsOperation.getCicsProgramName();
	}

	/**
	 * @return the COBOL source code for input data items.
	 * @throws CobolCodeGenException if code generation fails
	 */
	public String getInputStructuresCode() throws CobolCodeGenException {
		return getStructuresCode(Direction.INPUT);
	}

	/**
	 * @return the COBOL source code for output data items.
	 * @throws CobolCodeGenException if code generation fails
	 */
	public String getOutputStructuresCode() throws CobolCodeGenException {
		return getStructuresCode(Direction.OUTPUT);
	}

	/**
	 * Generates COBOL code for data structures. The result is a a
	 * concatenation of the code for each of the inner structures.
	 * @param direction either input or output
	 * @return the COBOL source code for data items
	 * @throws CobolCodeGenException if code generation fails
	 */
	private String getStructuresCode(
			final Direction direction) throws CobolCodeGenException {
		StringBuilder sb = new StringBuilder();
		List < CixsStructure > structures = null;

		if (direction == Direction.INPUT) {
			structures = mCixsOperation.getInput();
		} else {
			structures = mCixsOperation.getOutput();
		}
		for (CixsStructure structure : structures) {
			sb.append(getStructureCode(structure));
		}
		return sb.toString();
	}

	/**
	 * Using the <code>cobcgen</code> utility, this will use reflection
	 * to instantiate a Jaxb object corresponding to the structure
	 * received and then generate COBOL data description code using
	 * the COBOL annotations in the jaxb class. 
	 * @param structure the structure for which code is to be generated
	 * @return data description COBOL source code for the structure
	 * @throws CobolCodeGenException if code generation fails
	 */
	public String getStructureCode(
			final CixsStructure structure) throws CobolCodeGenException {
		try {
			Object objectFactory = getJaxbObjectFactory(
					structure.getJaxbPackageName());
			String className = getClassName(structure.getJaxbPackageName(),
					structure.getJaxbType());
			Class < ? > clazz = Class.forName(className);
			CComplexReflectBinding ccem = new CComplexReflectBinding(
					objectFactory, clazz);
			String cobolRootName = structure.getCobolRootDataItemName();
			if (cobolRootName == null || cobolRootName.length() == 0) {
				cobolRootName = mCobolNameResolver.getName(
						structure.getJaxbType());
			}
			ccem.setCobolName(cobolRootName);
			StringWriter writer = new StringWriter();
			BufferedWriter bufWriter = new BufferedWriter(writer);
			CobolGenVisitor cev = new CobolGenVisitor(5, 5, bufWriter);
			ccem.accept(cev);
			bufWriter.flush();
			return writer.toString();
		} catch (ReflectBindingException e) {
			throw new CobolCodeGenException(e);
		} catch (HostException e) {
			throw new CobolCodeGenException(e);
		} catch (IOException e) {
			throw new CobolCodeGenException(e);
		} catch (ClassNotFoundException e) {
			throw new CobolCodeGenException(e);
		} catch (CobolNameResolverException e) {
			throw new CobolCodeGenException(e);
		}
	}

	/**
	 * Returns a JAXB Object factory instance.
	 * @param jaxbPackageName the jaxb package name
	 * @return the object factory
	 * @throws CobolCodeGenException if object factory cannot be
	 *  instantiated
	 */
	private final Object getJaxbObjectFactory(final String jaxbPackageName)
	throws CobolCodeGenException {
		try {
			String className = getClassName(jaxbPackageName, "ObjectFactory");
			Class < ? > clazz = Class.forName(className);
			return clazz.newInstance();
		} catch (ClassNotFoundException e) {
			throw new CobolCodeGenException(e);
		} catch (InstantiationException e) {
			throw new CobolCodeGenException(e);
		} catch (IllegalAccessException e) {
			throw new CobolCodeGenException(e);
		}
	}

	/**
	 * Returns a fully qualified class name
	 * @param packageName the package name (can be null)
	 * @param typeName the type name
	 * @return a fully qualified class name
	 */
	private String getClassName(
			final String packageName, final String typeName) {
		String className = null;
		if (packageName == null || packageName.length() == 0) {
			className = "";
		} else {
			className = packageName + '.';
		}
		className += typeName;
		return className;
	}
	
	/**
	 * @return the URI that the host must use to reach the remote service
	 */
	public final String getServiceURI() {
		return mServiceURI;
	}

	/**
	 * @param serviceURI the URI that the host must use to reach the remote
	 *  service to set
	 */
	public final void setServiceURI(final String serviceURI) {
		mServiceURI = serviceURI;
	}

	/**
	 * @return the User ID to present remote service. If no user ID was set,
	 * this defaults to 8 space characters as a COBOL default.
	 */
	public final String getServiceUserId() {
		if (mServiceUserId == null || mServiceUserId.length() == 0) {
			return "        ";
		}
		return mServiceUserId;
	}

	/**
	 * @param serviceUserId the User ID to present remote service to set
	 */
	public final void setServiceUserId(final String serviceUserId) {
		mServiceUserId = serviceUserId;
	}

	/**
	 * @return the Password to present remote service. If no password was set,
	 * this defaults to 8 space characters as a COBOL default.
	 */
	public final String getServicePassword() {
		if (mServicePassword == null || mServicePassword.length() == 0) {
			return "        ";
		}
		return mServicePassword;
	}

	/**
	 * @param servicePassword the Password to present remote service to set
	 */
	public final void setServicePassword(final String servicePassword) {
		mServicePassword = servicePassword;
	}

	/**
	 * @return the Name of the remote service. By default,
	 * this will return the operation name.
	 */
	public final String getServiceName() {
		if (mServiceName == null || mServiceName.length() == 0) {
			return mCixsOperation.getName();
		}
		return mServiceName;
	}

	/**
	 * @param serviceName the Name of the remote service to set.
	 */
	public final void setServiceName(final String serviceName) {
		mServiceName = serviceName;
	}

	/**
	 * @return the Operation
	 */
	public final CixsOperation getCixsOperation() {
		return mCixsOperation;
	}

	/**
	 * @param cixsOperation the Operation to set
	 */
	public final void setCixsOperation(final CixsOperation cixsOperation) {
		mCixsOperation = cixsOperation;
	}

}

package com.legstar.coxb.cob2trans;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.Project;
import org.apache.ws.commons.schema.XmlSchema;
import org.apache.ws.commons.schema.XmlSchemaCollection;
import org.apache.ws.commons.schema.XmlSchemaComplexType;
import org.apache.ws.commons.schema.XmlSchemaElement;
import org.apache.ws.commons.schema.XmlSchemaObjectTable;

import com.legstar.antlr.RecognizerException;
import com.legstar.cob2xsd.Cob2XsdModel;
import com.legstar.cob2xsd.CobolStructureToXsd;
import com.legstar.cob2xsd.XsdGenerationException;
import com.legstar.coxb.gen.CoxbBindingGenerator;
import com.legstar.coxb.gen.CoxbGenModel;
import com.legstar.coxb.util.ClassLoadingException;
import com.legstar.coxb.util.ClassUtil;
import com.legstar.coxb.util.NameUtil;
import com.legstar.jaxb.gen.CobolJAXBGenerator;
import com.legstar.jaxb.gen.JaxbGenModel;

/**
 * Transformers Generator.
 * <p/>
 * Complete generation from COBOL fragment to Transformers ready to deploy.
 * <p/>
 * This is meant for environments with poor development capacities where we need
 * to hide the details of the generation process.
 * <p/>
 * For full flexibility, developers should use the individual generators
 * instead.
 * <p/>
 * The generation goes through these steps:
 * <ul>
 * <li>Turn COBOL fragment to XSD</li>
 * <li>Generate JAXB classes from XSD</li>
 * <li>Compile JAXB classes</li>
 * <li>Generate binding classes and Transformers (COXB classes)</li>
 * <li>Compile COXB classes</li>
 * <li>Bundle all compiled classes into a jar archive</li>
 * </ul>
 */
public class Cob2TransGenerator {

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /** Java compiler class name. */
    private static final String COMPILER = "com.sun.tools.javac.Main";

    /** Options set. */
    private Cob2TransModel _model;

    /** Total number of steps this generator performs. */
    public static final int TOTAL_STEPS = 6;

    /** A list of listeners to notify for progress. */
    private List < Cob2TransListener > _listeners;

    /** */
    private AtomicBoolean _interrupted = new AtomicBoolean();

    /**
     * Create an instance of a Transformer generator.
     * 
     * @param model the options in effect
     */
    public Cob2TransGenerator(final Cob2TransModel model) {
        _model = model;
    }

    /**
     * From a COBOL fragment, generates Transformers and bundles them in a jar
     * archive.
     * 
     * @param cobolFile the COBOL fragment containing data items descriptions
     * @param targetDir a target folder to produce artifacts
     * @return intermediate and final results including a jar archive ready to
     *         deploy
     * @throws Cob2TransException if generation fails
     */
    public Cob2TransResult generate(final File cobolFile,
            final File targetDir)
            throws Cob2TransException {
        return generate(cobolFile, null, targetDir);
    }

    /**
     * From a COBOL fragment, generates Transformers and bundles them in a jar
     * archive.
     * 
     * @param cobolFile the COBOL fragment containing data items descriptions
     * @param cobolSourceFileEncoding the character set used to encode the COBOL
     *            source file
     * @param targetDir a target folder to produce artifacts
     * @return intermediate and final results including a jar archive ready to
     *         deploy
     * @throws Cob2TransException if generation fails
     */
    public Cob2TransResult generate(final File cobolFile,
            final String cobolSourceFileEncoding,
            final File targetDir)
            throws Cob2TransException {
        String baseName = getBaseName(cobolFile);
        return generate(cobolFile, cobolSourceFileEncoding, baseName, targetDir);
    }

    /**
     * From a COBOL fragment, generates Transformers and bundles them in a jar
     * archive.
     * 
     * @param cobolFile the COBOL fragment containing data items descriptions
     * @param cobolSourceFileEncoding the character set used to encode the COBOL
     *            source file
     * @param targetDir a target folder to produce artifacts
     * @param baseName A name that can be used to identify generated artifacts
     * @return intermediate and final results including a jar archive ready to
     *         deploy
     * @throws Cob2TransException if generation fails
     */
    public Cob2TransResult generate(final File cobolFile,
            final String cobolSourceFileEncoding,
            final String baseName,
            final File targetDir)
            throws Cob2TransException {

        int stepNumber = 0;
        Cob2TransResult result = new Cob2TransResult();
        String eventDescription;

        Cob2TransDirs dirs = prepareTarget(targetDir, baseName, getModel());

        eventDescription = "Translate COBOL file '" + cobolFile
                + "' to XML Schema";
        fireEvent(++stepNumber, eventDescription,
                Cob2TransEvent.EventType.START);
        result.cob2xsdResult = cob2xsd(cobolFile,
                cobolSourceFileEncoding, baseName,
                dirs.getXsdDir(), getModel().getCob2XsdModel());
        File xsdFile = result.cob2xsdResult.xsdFile;
        for (String errorMessage : result.cob2xsdResult.errorHistory) {
            _log.warn(errorMessage);
        }
        fireEvent(stepNumber, eventDescription, Cob2TransEvent.EventType.STOP);

        eventDescription = "Generate JAXB classes for XML Schema '" + xsdFile
                + "'";
        fireEvent(++stepNumber, eventDescription,
                Cob2TransEvent.EventType.START);
        jaxbgen(xsdFile, dirs.getSrcDir(), getModel().getJaxbGenModel());
        fireEvent(stepNumber, eventDescription, Cob2TransEvent.EventType.STOP);

        eventDescription = "Compile JAXB classes in '" + dirs.getSrcDir() + "'";
        fireEvent(++stepNumber, eventDescription,
                Cob2TransEvent.EventType.START);
        String jaxbCompileResult = compile(dirs.getSrcDir(), dirs.getBinDir(),
                _log.isDebugEnabled());
        if (_log.isDebugEnabled()) {
            _log.debug(jaxbCompileResult);
        }
        fireEvent(stepNumber, eventDescription, Cob2TransEvent.EventType.STOP);

        eventDescription = "Generate COXB classes";
        fireEvent(++stepNumber, eventDescription,
                Cob2TransEvent.EventType.START);
        result.coxbgenResult = coxbgen(xsdFile,
                getModel().getCob2XsdModel().getXsdEncoding(),
                dirs.getSrcDir(),
                dirs.getBinDir(),
                getModel().getJaxbGenModel(),
                getModel().getCoxbGenModel());
        if (_log.isDebugEnabled()) {
            _log.debug("Root class names successfully processed: "
                    + result.coxbgenResult.rootClassNames);
        }
        fireEvent(stepNumber, eventDescription, Cob2TransEvent.EventType.STOP);

        eventDescription = "Compile COXB classes in " + dirs.getSrcDir();
        fireEvent(++stepNumber, eventDescription,
                Cob2TransEvent.EventType.START);
        String coxbCompileResult = compile(dirs.getSrcDir(), dirs.getBinDir(),
                _log.isDebugEnabled());
        if (_log.isDebugEnabled()) {
            _log.debug(coxbCompileResult);
        }
        fireEvent(stepNumber, eventDescription, Cob2TransEvent.EventType.STOP);

        eventDescription = "Create archive";
        fireEvent(++stepNumber, eventDescription,
                Cob2TransEvent.EventType.START);
        result.jarFile = jar(dirs.getDistDir(), dirs.getBinDir(), baseName);
        if (_log.isDebugEnabled()) {
            _log.debug("Archive " + result.jarFile + " successfully created");
        }
        fireEvent(stepNumber, eventDescription, Cob2TransEvent.EventType.STOP);

        return result;

    }

    /**
     * Notify listeners of a step event.
     * 
     * @param stepNumber the step number
     * @param description the step description
     * @param eventType the type of event
     * @throws Cob2TransException if firing fails
     */
    protected void fireEvent(
            final int stepNumber,
            final String description,
            final Cob2TransEvent.EventType eventType) throws Cob2TransException {
        if (isInterrupted()) {
            throw new Cob2TransException("interrupted");
        }
        Cob2TransEvent event = new Cob2TransEvent(this, stepNumber,
                description, eventType);
        if (_log.isInfoEnabled()) {
            _log.info(event);
        }
        if (_listeners != null) {
            for (Cob2TransListener listener : _listeners) {
                listener.stepPerformed(event);
            }
        }
    }

    /**
     * Create the generation target folder and sub-folders.
     * <p/>
     * What we do here is that we create a tree that guarantees that the
     * artifacts produced for one COBOL file do not get mixed with artifacts
     * from another.
     * <p>
     * For this we use the baseName that will group all artifacts for a given
     * COBOL file as a folder and than create the folders given in the model as
     * sub folders of the baseName.
     * 
     * @param targetDir
     *            the uber target folder
     * @param baseName A name that can be used to identify generated artifacts
     * @param model
     *            the options in effect
     * @return a class holding the target subfolders
     * @throws Cob2TransException
     *             if target folders cannot be created
     */
    public static Cob2TransDirs prepareTarget(
            final File targetDir,
            final String baseName,
            final Cob2TransModel model) throws Cob2TransException {
        try {
            Cob2TransDirs targetDirs = new Cob2TransDirs();
            targetDirs.setXsdDir(new File(targetDir, baseName + '/'
                    + model.getXsdFolderName()));
            targetDirs.setSrcDir(new File(targetDir, baseName + '/'
                    + model.getSrcFolderName()));
            targetDirs.setBinDir(new File(targetDir, baseName + '/'
                    + model.getBinFolderName()));
            targetDirs.setDistDir(new File(targetDir, baseName + '/'
                    + model.getDistFolderName()));

            if (model.isCleanFolders()) {
                targetDirs.clean();
            } else {
                targetDirs.create();
            }
            return targetDirs;

        } catch (IOException e) {
            throw new Cob2TransException(e);
        }
    }

    /**
     * This class holds the outcome of a COBOL to Transformers execution.
     * 
     */
    public static class Cob2TransResult {

        /** The intermediate result of the COBOL to XML Schema translation. */
        public Cob2XsdResult cob2xsdResult;

        /** The intermediate result of the COBOL binding generator. */
        public CoxbgenResult coxbgenResult;

        /** The final jar produced. */
        public File jarFile;

    }

    /**
     * Translate a COBOL fragment to XML Schema.
     * We consider the targetNamespace passed as an option to be a prefix to
     * which we append the COBOL file name to form the actual targetNamespace.
     * <p/>
     * Here we temporarily update the model with the actual targetNamespace and
     * preserve the targetNamespace prefix so we can restore the model which can
     * be reused several times.
     * 
     * @param cobolFile the COBOL file
     * @param cobolSourceFileEncoding the character set used to encode the COBOL
     *            source file
     * @param baseName A name, derived from the COBOL file name, that can be
     *            used to identify generated artifacts
     * @param xsdDir the target XML Schema folder
     * @param model the COBOL to XSD options set
     * @return various results including an XML Schema file
     * @throws Cob2TransException if generation fails
     */
    public static Cob2XsdResult cob2xsd(
            final File cobolFile,
            final String cobolSourceFileEncoding,
            final String baseName,
            final File xsdDir,
            final Cob2XsdModel model)
            throws Cob2TransException {
        try {
            Cob2XsdResult result = new Cob2XsdResult();
            model.setAddLegStarAnnotations(true);

            // Update the targetNamespace to include the base name
            String targetNamespacePrefix = model.getTargetNamespace();
            String targetNamespace = getUniqueTargetNamespace(
                    baseName, targetNamespacePrefix);
            model.setTargetNamespace(targetNamespace);

            CobolStructureToXsd cobTranslator = new CobolStructureToXsd(model);
            result.xsdFile = cobTranslator.translate(cobolFile,
                    cobolSourceFileEncoding,
                    new File(xsdDir, baseName + ".xsd"));
            result.errorHistory = cobTranslator.getErrorHistory();
            result.targetNamespace = targetNamespace;

            // Restore targetNamespace to its original value
            model.setTargetNamespace(targetNamespacePrefix);
            return result;

        } catch (RecognizerException e) {
            throw new Cob2TransException(e);
        } catch (XsdGenerationException e) {
            throw new Cob2TransException(e);
        }
    }

    /**
     * TargetNamespace, if it is not null, is completed with the
     * baseName.
     * 
     * @param baseName A name, derived from the COBOL file name, that can be
     *            used to identify generated artifacts
     * @param targetNamespacePrefix the namespace prefix
     * @return the previous value of the model targetNamespace field
     */
    protected static String getUniqueTargetNamespace(final String baseName,
            final String targetNamespacePrefix) {

        if (targetNamespacePrefix != null
                && targetNamespacePrefix.length() > 0
                && !targetNamespacePrefix.endsWith(baseName)) {
            if (targetNamespacePrefix
                    .charAt(targetNamespacePrefix.length() - 1) == '/') {
                return targetNamespacePrefix + baseName;
            } else {
                return targetNamespacePrefix + '/' + baseName;
            }
        }
        return targetNamespacePrefix;
    }

    /**
     * Holds the results from the translator execution.
     * 
     */
    public static class Cob2XsdResult {
        /** XML Schema produced. */
        public File xsdFile;

        /** Recap of warnings encountered while parsing the COBOL source. */
        public List < String > errorHistory;

        /** The target namespace used for the XML Schema artifact. */
        public String targetNamespace;

    }

    /**
     * Generate JAXB classes from an XML Schema.
     * 
     * @param xsdFile
     *            the XML SChema file
     * @param srcDir
     *            the target source folder
     * @param model the options in effect
     */
    public static void jaxbgen(final File xsdFile, final File srcDir,
            final JaxbGenModel model) {
        CobolJAXBGenerator jaxbGenerator = new CobolJAXBGenerator(model);
        jaxbGenerator.setProject(new Project());
        jaxbGenerator.init();
        jaxbGenerator.getProject().fireBuildStarted();
        jaxbGenerator.setXsdFile(xsdFile);
        jaxbGenerator.setTargetDir(srcDir);
        jaxbGenerator.execute();
    }

    /**
     * Compiles java classes from source folder.
     * <p/>
     * The java compiler must be available from a JDK.
     * 
     * @param srcDir the source folder
     * @param binDir the binaries folder
     * @param verbose print out compiler diagnostic
     * @return the compiler output
     * @throws Cob2TransException if comilation fails
     */
    @SuppressWarnings("unchecked")
    public static String compile(final File srcDir, final File binDir,
            final boolean verbose)
            throws Cob2TransException {
        try {

            // Prepare compiler arguments

            Collection < File > javaSrcFiles = FileUtils.listFiles(srcDir,
                    new String[] { "java" }, true);

            String[] preArgs = null;
            if (verbose) {
                preArgs = new String[] { "-verbose", "-d", binDir.getPath() };
            } else {
                preArgs = new String[] { "-d", binDir.getPath() };
            }
            String[] compilerArgs = new String[javaSrcFiles.size()
                    + preArgs.length];

            System.arraycopy(preArgs, 0, compilerArgs, 0, preArgs.length);
            int i = preArgs.length;
            for (File file : javaSrcFiles) {
                compilerArgs[i] = file.getPath();
                i++;
            }

            StringWriter compilerOut = new StringWriter();

            // Load compiler dynamically
            Object compiler = null;
            URL[] urlToolsJar = getJDKURLs();
            if (urlToolsJar.length > 0) {
                URLClassLoader compileCl = new URLClassLoader(urlToolsJar,
                        Thread.currentThread().getContextClassLoader());

                Class < ? > compilerClass = compileCl.loadClass(COMPILER);
                compiler = compilerClass.newInstance();
            } else {
                compiler = ClassUtil.newObject(COMPILER);
            }

            // Get the compilation method and setup arguments
            Class < ? >[] param = { String[].class, PrintWriter.class };
            Method method = compiler.getClass().getMethod("compile", param);

            Object[] methodArgs = new Object[2];
            methodArgs[0] = compilerArgs;
            methodArgs[1] = new PrintWriter(compilerOut);

            // Invoke compilation and check return code
            int compileRc = (Integer) method.invoke(compiler, methodArgs);
            if (compileRc != 0) {
                throw new Cob2TransException(
                        "Failed to compile java code. "
                                + compilerOut.toString());
            }
            return compilerOut.toString();

        } catch (SecurityException e) {
            throw new Cob2TransException(e);
        } catch (IllegalArgumentException e) {
            throw new Cob2TransException(e);
        } catch (Cob2TransException e) {
            throw new Cob2TransException(e);
        } catch (ClassNotFoundException e) {
            throw new Cob2TransException(e);
        } catch (NoSuchMethodException e) {
            throw new Cob2TransException(e);
        } catch (IllegalAccessException e) {
            throw new Cob2TransException(e);
        } catch (InvocationTargetException e) {
            throw new Cob2TransException(e);
        } catch (InstantiationException e) {
            throw new Cob2TransException(e);
        } catch (ClassLoadingException e) {
            throw new Cob2TransException(e);
        }
    }

    /**
     * Locates the JDK URLs needed for compilation (Actually tools.jar).
     * 
     * @return an array of URLs
     * @throws Cob2TransException if JDK cannot be located
     */
    public static URL[] getJDKURLs() throws Cob2TransException {
        try {
            // Already on the classpath
            ClassUtil.newObject(COMPILER);
            return new URL[0];
        } catch (ClassLoadingException e1) {
            try {
                File jreHome = new File(System.getProperty("java.home"));
                if (jreHome != null) {
                    File toolsjar = new File(jreHome, "../lib/tools.jar");
                    if (toolsjar.exists()) {
                        return new URL[] { toolsjar.toURI().toURL() };
                    }
                }

                String javaHome = System.getenv("JAVA_HOME");
                if (javaHome != null) {
                    File toolsjar = new File(javaHome + "/lib/tools.jar");
                    if (toolsjar.exists()) {
                        return new URL[] { toolsjar.toURI().toURL() };
                    }
                }

                throw new Cob2TransException("Unable to locate tools.jar."
                        + " Please run under a JDK"
                        + " or set JAVA_HOME to point to a JDK.");

            } catch (MalformedURLException e) {
                throw new Cob2TransException(e);
            }
        }

    }

    /**
     * Generate binding classes for all root classes.
     * <p/>
     * Root classes are identified using the XML Schema elements.
     * 
     * @param xsdFile the XML Schema
     * @param xsdEncoding the XML Schema encoding character set
     * @param srcDir the target source folder
     * @param binDir the binaries folder where JAXB classes were compiled
     * @param jaxbModel the jaxbgen options in effect
     * @param model the options set
     * @return various results including the list of JAXB root class names
     *         processed
     * @throws Cob2TransException if generation fails
     */
    @SuppressWarnings("unchecked")
    public static CoxbgenResult coxbgen(
            final File xsdFile,
            final String xsdEncoding,
            final File srcDir,
            final File binDir,
            final JaxbGenModel jaxbModel,
            final CoxbGenModel model) throws Cob2TransException {
        ClassLoader previousCl = Thread.currentThread().getContextClassLoader();

        try {
            CoxbgenResult result = new CoxbgenResult();
            result.rootClassNames = new ArrayList < String >();

            URL[] urlBinFiles = new URL[] { binDir.toURI().toURL() };

            CoxbBindingGenerator coxbGenerator = new CoxbBindingGenerator(model);
            coxbGenerator.setXsdFile(xsdFile);
            coxbGenerator.setJaxbBinDir(binDir);
            coxbGenerator.setTargetDir(srcDir);

            XmlSchemaCollection schemaCol = new XmlSchemaCollection();
            XmlSchema xsd = schemaCol.read(new InputStreamReader(
                    new FileInputStream(xsdFile), xsdEncoding), null);
            XmlSchemaObjectTable schemaElements = xsd.getElements();
            Iterator it = schemaElements.getValues();
            while (it.hasNext()) {
                Object element = it.next();
                if (element instanceof XmlSchemaElement) {
                    Object type = ((XmlSchemaElement) element).getSchemaType();
                    if (type instanceof XmlSchemaComplexType) {
                        String rootClassName = NameUtil
                                .toClassName(((XmlSchemaComplexType) type)
                                        .getName());
                        if (jaxbModel.getTypeNamePrefix() != null) {
                            rootClassName = jaxbModel.getTypeNamePrefix()
                                    + rootClassName;
                        }
                        if (jaxbModel.getTypeNameSuffix() != null) {
                            rootClassName += jaxbModel.getTypeNameSuffix();
                        }
                        coxbGenerator.addJaxbRootClass(rootClassName);
                        result.rootClassNames.add(rootClassName);
                    }
                }
            }

            URLClassLoader coxbCl = new URLClassLoader(urlBinFiles, previousCl);
            Thread.currentThread().setContextClassLoader(coxbCl);

            coxbGenerator.execute();

            result.jaxbPackageName = coxbGenerator.getJaxbPackageName();
            result.coxbPackageName = coxbGenerator.getCoxbPackageName();

            return result;

        } catch (MalformedURLException e) {
            throw new Cob2TransException(e);
        } catch (UnsupportedEncodingException e) {
            throw new Cob2TransException(e);
        } catch (FileNotFoundException e) {
            throw new Cob2TransException(e);
        } finally {
            Thread.currentThread().setContextClassLoader(previousCl);
        }
    }

    /**
     * This class holds the outcome of a COBOL Binding generator execution.
     * 
     */
    public static class CoxbgenResult {

        /** The JAXB package name. */
        public String jaxbPackageName;

        /** The COXB package name. */
        public String coxbPackageName;

        /** The list of JAXB root class names. */
        public List < String > rootClassNames;

    }

    /**
     * Create a jar file containing all compiled code ready for distribution.
     * 
     * @param distDir the distribution folder
     * @param binDir the binaries folder where classes were compiled
     * @param baseName A name, derived from the COBOL file name, that can be
     *            used to identify generated artifacts
     * @return a jar archive holding the Transformers
     * @throws Cob2TransException if creating the archive fails
     */
    public static File jar(final File distDir, final File binDir,
            final String baseName) throws Cob2TransException {
        try {
            Manifest manifest = new Manifest();
            manifest.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION,
                    "1.0");
            String jarFileName = baseName + ".jar";
            File jarFile = new File(distDir, jarFileName);
            JarOutputStream jarStream = new JarOutputStream(
                    new FileOutputStream(jarFile), manifest);
            for (File fileEntry : binDir.listFiles()) {
                addJarEntry(jarStream, binDir, fileEntry);
            }
            jarStream.close();
            return jarFile;
        } catch (FileNotFoundException e) {
            throw new Cob2TransException(e);
        } catch (IOException e) {
            throw new Cob2TransException(e);
        }
    }

    /**
     * Add an entry to a jar file. Entry can be a folder or a file.
     * 
     * @param jarStream
     *            the jar stream
     * @param binDir the binaries folder
     * @param fileEntry
     *            the file entry (folder or file)
     * @throws Cob2TransException
     *             if adding entry fails
     */
    protected static void addJarEntry(
            final JarOutputStream jarStream,
            final File binDir,
            final File fileEntry) throws Cob2TransException {
        BufferedInputStream in = null;
        byte[] buffer = new byte[10240];
        try {
            JarEntry jarEntry = new JarEntry(getJarEntryName(binDir, fileEntry));
            jarEntry.setTime(fileEntry.lastModified());
            jarStream.putNextEntry(jarEntry);

            if (fileEntry.isDirectory()) {
                jarStream.closeEntry();
                for (File nestedFile : fileEntry.listFiles()) {
                    addJarEntry(jarStream, binDir, nestedFile);
                }
            } else {
                in = new BufferedInputStream(new FileInputStream(fileEntry));
                while (true) {
                    int nRead = in.read(buffer, 0, buffer.length);
                    if (nRead <= 0) {
                        break;
                    }
                    jarStream.write(buffer, 0, nRead);
                }
                jarStream.closeEntry();
            }
        } catch (IOException e) {
            throw new Cob2TransException(e);
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                    throw new Cob2TransException(e);
                }
            }
        }
    }

    /**
     * We don't want the parent binaries folder to be part of the jar file.
     * <p/>
     * Names are therefore created by removing the parent tree folder to keep
     * the relative path of files only.
     * <p/>
     * Names are also UNIX normalized.
     * 
     * @param binDir the binaries folder
     * @param fileEntry a file entry within the binaries folder
     * @return a suitable name for a jar entry
     */
    protected static String getJarEntryName(final File binDir,
            final File fileEntry) {
        String entryName = fileEntry.getPath();
        int pos = entryName.indexOf(binDir.getPath());
        if (pos == 0) {
            entryName = entryName.substring(binDir.getPath().length());
        }
        entryName = entryName.replace("\\", "/");
        if (fileEntry.isDirectory() && !entryName.endsWith("/")) {
            entryName += "/";
        }
        if (entryName.charAt(0) == '/') {
            entryName = entryName.substring(1);
        }
        return entryName;
    }

    /**
     * Creates a name to identify artifacts produced from a COBOL file.
     * 
     * @param cobolFile the COBOL file
     * @return an identifier for all artifacts produced
     */
    public static String getBaseName(final File cobolFile) {
        return FilenameUtils.getBaseName(cobolFile.getName()).toLowerCase();
    }

    /**
     * @return the parameter set
     */
    public Cob2TransModel getModel() {
        return _model;
    }

    /**
     * Add a listener for steps events.
     * 
     * @param listener a new listener
     */
    public void addCob2TransListener(final Cob2TransListener listener) {
        if (_listeners == null) {
            _listeners = new CopyOnWriteArrayList < Cob2TransListener >();
        }
        _listeners.add(listener);
    }

    /**
     * Remove a listener.
     * 
     * @param listener the listener to remove
     */
    public void removeCob2TransListener(final Cob2TransListener listener) {
        if (_listeners != null) {
            _listeners.remove(listener);
        }
    }

    /**
     * Used to stop the generation process.
     */
    public void interrupt() {
        _interrupted.getAndSet(true);
    }

    /**
     * @return true if generator is forcefully stopped
     */
    public boolean isInterrupted() {
        return _interrupted.get();
    }

}

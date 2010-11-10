package com.legstar.coxb.cob2trans.exe;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.PosixParser;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.cob2xsd.Cob2XsdModel;
import com.legstar.coxb.cob2trans.Cob2TransException;
import com.legstar.coxb.cob2trans.Cob2TransGenerator;
import com.legstar.coxb.cob2trans.Cob2TransModel;
import com.legstar.coxb.cob2trans.Cob2TransGenerator.Cob2TransResult;

/**
 * COBOL structure to Transformers standalone executable.
 * <p/>
 * This is the main class for the executable jar. It takes options from the
 * command line and calls the {@link Cob2TransGenerator} API.
 * <p/>
 * Usage: <code>
 * java -jar legstar-cob2trans-x.y.z-exe.jar -i&lt;input file&gt; -o&lt;output folder&gt;
 * </code>
 * 
 */
public class Cob2TransGeneratorMain {

    /** The version properties file name. */
    private static final String VERSION_FILE_NAME = "/com/legstar/coxb/cob2trans/exe/version.properties";

    /** The default input. */
    private static final String DEFAULT_INPUT_FOLDER = "cobol";

    /** The default output. */
    private static final String DEFAULT_OUTPUT_FOLDER = "target";

    /** The default output. */
    private static final String DEFAULT_CONFIG_FILE = "conf/cob2trans.properties";

    /** A file containing parameters. */
    private File _configFile;

    /**
     * A file containing COBOL code to generate Transformers. Defaults to cobol
     * relative folder.
     */
    private File _input;

    /** Character set used to encode the input COBOL source files. */
    private String _cobolSourceFileEncoding;

    /**
     * A folder containing generated artifacts. Defaults to target relative
     * folder.
     */
    private File _output;

    /**
     * A class path to use by compiler to locate dependencies.
     */
    private String _classpath;

    /** Set of generation options to use. */
    private Cob2TransModel _model;

    /** Line separator (OS specific). */
    public static final String LS = System.getProperty("line.separator");

    /** Logger. */
    private final Log _log = LogFactory.getLog(getClass());

    /**
     * @param args translator options. Provides help if no arguments passed.
     */
    public static void main(final String[] args) {
        Cob2TransGeneratorMain main = new Cob2TransGeneratorMain();
        main.execute(args);
    }

    /**
     * Process command line options and run generator.
     * <p/>
     * If no options are passed, prints the help. Help is also printed if the
     * command line options are invalid.
     * 
     * @param args generator options
     */
    public void execute(final String[] args) {
        try {
            Options options = createOptions();
            if (collectOptions(options, args)) {
                setDefaults();
                loadModel();
                execute(getInput(), getCobolSourceFileEncoding(), getOutput());
            }
        } catch (Exception e) {
            _log.error("Transformers generator failure", e);
            throw new RuntimeException(e);
        }
    }

    /**
     * Take arguments received on the command line and setup corresponding
     * options.
     * <p/>
     * No arguments is valid. It means use the defaults.
     * 
     * @param options the expected options
     * @param args the actual arguments received on the command line
     * @return true if arguments were valid
     * @throws Exception if something goes wrong while parsing arguments
     */
    protected boolean collectOptions(
            final Options options,
            final String[] args) throws Exception {
        if (args != null && args.length > 0) {
            CommandLineParser parser = new PosixParser();
            CommandLine line = parser.parse(options, args);
            return processLine(line, options);
        }
        return true;
    }

    /**
     * Make sure mandatory parameters have default values.
     */
    protected void setDefaults() {
        if (getConfigFile() == null) {
            setConfigFile(DEFAULT_CONFIG_FILE);
        }
        if (getInput() == null) {
            setInput(DEFAULT_INPUT_FOLDER);
        }
        if (getOutput() == null) {
            setOutput(DEFAULT_OUTPUT_FOLDER);
        }

    }

    /**
     * A configuration file is expected. We load it into the generator model.
     * 
     * @throws Cob2TransException
     *             if configuration file missing or file corrupt
     */
    protected void loadModel() throws Cob2TransException {
        try {
            if (getConfigFile() == null) {
                _model = new Cob2TransModel();
            } else {
                Properties config = new Properties();
                config.load(new FileInputStream(getConfigFile()));
                _model = new Cob2TransModel(config);
            }
        } catch (FileNotFoundException e) {
            throw new Cob2TransException(e);
        } catch (IOException e) {
            throw new Cob2TransException(e);
        }
    }

    /**
     * @param options options available
     * @throws Exception if help cannot be produced
     */
    protected void produceHelp(final Options options) throws Exception {
        HelpFormatter formatter = new HelpFormatter();
        String version = getVersion();
        formatter.printHelp("java -jar legstar-cob2trans-"
                + version.substring(0, version.indexOf(' '))
                + "-exe.jar followed by:", options);
    }

    /**
     * @return the command line options
     */
    protected Options createOptions() {
        Options options = new Options();

        Option version = new Option("v", "version", false,
                "print the version information and exit");
        options.addOption(version);

        Option help = new Option("h", "help", false,
                "print the options available");
        options.addOption(help);

        Option configFile = new Option("c", "config", true,
                "path to configuration file");
        options.addOption(configFile);

        Option input = new Option("i", "input", true,
                "file or folder holding the COBOL code to translate."
                        + " Name is relative or absolute");
        options.addOption(input);

        Option cobolSourceFileEncoding = new Option("e",
                "sourceEncoding", true,
                "Character set used for COBOL source files encoding");
        options.addOption(cobolSourceFileEncoding);

        Option output = new Option("o", "output", true,
                "folder receiving the generated artifacts");
        options.addOption(output);

        Option classpath = new Option("cp", "classpath", true,
                "classpath to use for generated classes compilation");
        options.addOption(classpath);

        return options;
    }

    /**
     * Process the command line options selected.
     * 
     * @param line the parsed command line
     * @param options available
     * @return false if processing needs to stop, true if its ok to continue
     * @throws Exception if line cannot be processed
     */
    protected boolean processLine(
            final CommandLine line,
            final Options options) throws Exception {
        if (line.hasOption("version")) {
            System.out.println("version " + getVersion());
            return false;
        }
        if (line.hasOption("help")) {
            produceHelp(options);
            return false;
        }
        if (line.hasOption("config")) {
            setConfigFile(line.getOptionValue("config").trim());
        }
        if (line.hasOption("input")) {
            setInput(line.getOptionValue("input").trim());
        }
        if (line.hasOption("cobolSourceFileEncoding")) {
            setCobolSourceFileEncoding(line.getOptionValue(
                    "cobolSourceFileEncoding").trim());
        }
        if (line.hasOption("output")) {
            setOutput(line.getOptionValue("output").trim());
        }
        if (line.hasOption("classpath")) {
            setClasspath(line.getOptionValue("classpath").trim());
        }

        return true;
    }

    /**
     * Translate a single file or all files from an input folder.
     * Place results in the output folder.
     * 
     * @param input the input COBOL file or folder
     * @param cobolSourceFileEncoding the input file character set
     * @param target the output folder or file where XML schema file must go
     * @throws Cob2TransException if generation fails
     */
    protected void execute(final File input,
            final String cobolSourceFileEncoding, final File target)
            throws Cob2TransException {

        _log.info("Started generation from COBOL to Transformers");
        _log.info("Configuration file     : " + getConfigFile());
        _log.info("Taking COBOL from      : " + input);
        _log.info("COBOL files encoding   : " + cobolSourceFileEncoding);
        _log.info("Output Transformers to : " + target);
        _log.info("Options in effect      : " + getModel().toString());
        if (input.isFile()) {
            generate(input, cobolSourceFileEncoding, target);
        } else {
            for (File cobolFile : input.listFiles()) {
                if (cobolFile.isFile()) {
                    generate(cobolFile, cobolSourceFileEncoding, target);
                }
            }
        }
        _log.info("Finished generation");

    }

    /**
     * Generate Transformers for a single COBOL source file.
     * 
     * @param cobolFile COBOL source file
     * @param cobolSourceFileEncoding COBOL source file character encoding
     * @param target target file or folder
     * @throws Cob2TransException if generation fails
     */
    protected void generate(
            final File cobolFile,
            final String cobolSourceFileEncoding,
            final File target) throws Cob2TransException {
        _log.info("Generation started for: " + cobolFile);

        Cob2TransGenerator cob2trans = new Cob2TransGenerator(getModel());
        Cob2TransResult result = cob2trans.generate(
                cobolFile,
                cobolSourceFileEncoding,
                target,
                getClasspath());

        _log.info("Result jar archive is '" + result.jarFile + "'");
    }

    /**
     * Pick up the version from the properties file.
     * 
     * @return the product version
     * @throws IOException if version cannot be identified
     */
    protected String getVersion() throws IOException {
        InputStream stream = null;
        try {
            Properties version = new Properties();
            stream = Cob2TransGeneratorMain.class.getResourceAsStream(
                            VERSION_FILE_NAME);
            version.load(stream);
            return version.getProperty("version");
        } finally {
            if (stream != null) {
                stream.close();
            }
        }
    }

    /**
     * @return the file containing parameters
     */
    public File getConfigFile() {
        return _configFile;
    }

    /**
     * Check the config parameter and keep it only if it is valid.
     * <p/>
     * If the default properties file is not there, we run with the defaults.
     * 
     * @param config a file name (relative or absolute)
     */
    public void setConfigFile(final String config) {
        if (config == null) {
            throw (new IllegalArgumentException(
                    "You must provide a configuration file"));
        }
        File file = new File(config);
        if (file.exists()) {
            if (file.isDirectory()) {
                throw new IllegalArgumentException("Folder '" + config
                        + "' is not a configuration file");
            }
        } else {
            if (config.equals(DEFAULT_CONFIG_FILE)) {
                file = null;
            } else {
                throw new IllegalArgumentException("Configuration file '"
                        + config
                        + "' not found");
            }
        }
        setConfigFile(file);
    }

    /**
     * @return the file or folder containing COBOL code to translate to XSD
     */
    public File getInput() {
        return _input;
    }

    /**
     * Check the input parameter and keep it only if it is valid.
     * 
     * @param input a file or folder name (relative or absolute)
     */
    public void setInput(final String input) {
        if (input == null) {
            throw (new IllegalArgumentException(
                    "You must provide a COBOL source folder or file"));
        }
        File file = new File(input);
        if (file.exists()) {
            if (file.isDirectory() && file.list().length == 0) {
                throw new IllegalArgumentException("Folder '" + input
                        + "' is empty");
            }
        } else {
            throw new IllegalArgumentException("Input file or folder '" + input
                    + "' not found");
        }
        _input = file;
    }

    /**
     * @param configFile the file containing parameters to set
     */
    public void setConfigFile(final File configFile) {
        _configFile = configFile;
    }

    /**
     * Gather all parameters into a context object.
     * 
     * @return a parameter context to be used throughout all code
     */
    public Cob2TransModel getModel() {
        return _model;
    }

    /**
     * Parameters specific to COBOL to XML Schema translation.
     * 
     * @return parameters specific to COBOL to XML Schema translation
     */
    public Cob2XsdModel getCob2XsdModel() {
        return _model.getCob2XsdModel();
    }

    /**
     * @return the character set used to encode the input COBOL source files
     */
    public String getCobolSourceFileEncoding() {
        return _cobolSourceFileEncoding;
    }

    /**
     * @param cobolSourceFileEncoding the character set used to encode the input
     *            COBOL source files
     */
    public void setCobolSourceFileEncoding(final String cobolSourceFileEncoding) {
        _cobolSourceFileEncoding = cobolSourceFileEncoding;
    }

    /**
     * @return the folder containing translated XML Schema
     */
    public File getOutput() {
        return _output;
    }

    /**
     * Check the output parameter and keep it only if it is valid.
     * 
     * @param output a file or folder name (relative or absolute)
     */
    public void setOutput(final String output) {
        if (output == null) {
            throw (new IllegalArgumentException(
                    "You must provide a target directory or file"));
        }
        _output = new File(output);
    }

    /**
     * @return the class path to use by compiler to locate dependencies
     */
    public String getClasspath() {
        return _classpath;
    }

    /**
     * @param classpath the class path to use by compiler to locate dependencies
     *            to set
     */
    public void setClasspath(String classpath) {
        _classpath = classpath;
    }

}

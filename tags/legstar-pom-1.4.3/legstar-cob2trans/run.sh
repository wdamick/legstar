#! /bin/sh
##   ---------------------------------------------------------------------------
##   Generate Transformers from COBOL structures
##   -------------------------------------------
##   type run -h to get help on available options
##   ---------------------------------------------------------------------------

##   Use the following to set your own JVM arguments
JVM_ARGS=

##   Update the log4j configuration to set debug mode
JVM_ARGS="$JVM_ARGS -Dlog4j.configuration=file:conf/log4j.properties"

java $JVM_ARGS -jar legstar-cob2trans-${project.version}-exe.jar "$@"

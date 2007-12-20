#!/usr/bin/perl -w
#-------------------------------------------------------------
# Receives Cobol statements from a posted textarea and       -
# generates an XSD in an XML file.                                               -
#-------------------------------------------------------------
use strict;
use File::Temp qw/ tempdir mktemp/;
use CGI;
use CGI::Carp qw(fatalsToBrowser);    # Remove for production use
# This is needed because of the command execution
$ENV{'PATH'} = 'C:\WINDOWS;C:\WINDOWS\System32;C:\Program Files\Apache Software Foundation\Apache2.2\cgi-bin\legstar\legstar-schemagen-web';

#-------------------------------------------------------------
# Functions prototypes                                       -
#-------------------------------------------------------------
sub validate_input($);
sub save_file($);
sub gen_xsd($$);

#-------------------------------------------------------------
# Main section                                               -
#-------------------------------------------------------------
my $cgi = new CGI;
my $g_tempdir = tempdir( 'temp/XXXXXXXX', CLEANUP => 1);

print $cgi->header('text/xml');
validate_input($cgi);    
gen_xsd($cgi, save_file($cgi));

exit (0);

#-------------------------------------------------------------
# Validate the data received                                 -
#-------------------------------------------------------------
sub validate_input($) {
    my ($q) = @_;
#
# If nothing was passed, signal error and exit
#
    if (!$q->param() || !$q->param('cobeditArea')) {
        die("No Cobol content was posted. Please try again.");
    }
}

#-------------------------------------------------------------
# Save the cobol content into a temporary file               -
#-------------------------------------------------------------
sub save_file($) {
    my ($q) = @_;
    my $cobContent = $q->param('cobeditArea');
    my $tempCobFile = mktemp("$g_tempdir/cobXXXXXXXX");

    open (OUTFILE, ">", "$tempCobFile")
          or die "Couldn't open $tempCobFile for writing: $!";
    print OUTFILE $cobContent;
    close OUTFILE or die "Couldn't close $tempCobFile: $!";
    
    return $tempCobFile;

}

#-------------------------------------------------------------
# Generate the XSD                                           -
#-------------------------------------------------------------
sub gen_xsd($$) {
    my $q = @_[0];
    my $cobFileName = @_[1];
    die "Read failure" unless defined($cobFileName);
    
    #
    # Parse and create schema 
    #
    my $xsdFileName = "temp/" . "$$" . "$^T" . ".xml"; 
    open (IN, "PARSTSTS.exe $cobFileName $xsdFileName |")
        or die "failed to execute PARSTSTS.exe $cobFileName $xsdFileName: $!";
    close IN;
    
    open (XSDFILE, "<", "$xsdFileName")
        or die "Couldn't open $xsdFileName for reading: $!";
    while(<XSDFILE>) {
        print (<XSDFILE>);
    }
    close XSDFILE or die "Couldn't close $xsdFileName: $!";

}

    
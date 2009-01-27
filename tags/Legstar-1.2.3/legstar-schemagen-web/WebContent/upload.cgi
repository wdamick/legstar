#!/usr/bin/perl -w
#-------------------------------------------------------------
# Reads the content of an uploaded file and sends it back    -
# for editing.                                               -
#-------------------------------------------------------------
use strict;
use CGI;
use CGI::Carp qw(fatalsToBrowser);  # Remove for production use
$CGI::POST_MAX = 1024 * 100;  # maximum upload filesize is 100K

#-------------------------------------------------------------
# Functions prototypes                                       -
#-------------------------------------------------------------
sub validate_input($);

#-------------------------------------------------------------
# Main section                                               -
#-------------------------------------------------------------
my $cgi = new CGI;
my $buffer;
my $buffer_size = 1024;
my $bytesread;

validate_input($cgi);    
print $cgi->header("text/plain");
my $cobFileHandle = $cgi->upload('cobolFile');
while ($bytesread = read($cobFileHandle, $buffer, $buffer_size)) {
    print $buffer;
}
exit (0);

#-------------------------------------------------------------
# Validate the data received                                 -
#-------------------------------------------------------------
sub validate_input($) {
    my ($q) = @_;
#
# If nothing was uploaded, signal error and exit
#
    if (!$q->param()) {
        die("There was nothing to upload. Please try again.");
    }
#
# Look for uploads that exceed $CGI::POST_MAX
#

    if ($cgi->cgi_error()) {
        die($q->cgi_error());
    }
}

    
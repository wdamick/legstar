#!/usr/bin/perl -w
use CGI::Carp qw(fatalsToBrowser);    # Remove for production use
#$ENV{'PATH'} = 'C:\WINDOWS;C:\WINDOWS\System32;D:\Fady\sandbox\legstar-1.1.0\bin';
$ENV{'PATH'} = '/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin:/usr/X11R6/bin:/root/bin';
#$ENV{'TMPDIR'} = '/var/www/cgi-bin/schemagen-linux/temp';
print <<"EndOfHTML";
Content-type: text/html

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">
<HTML>
<HEAD>
<TITLE>Compile a C program</TITLE>
</HEAD>
<BODY BGCOLOR="#FFFFFF">
EndOfHTML

#open (IN, "cc -v -c schemagen-linux/c/PARSTSTS.c -Ischemagen-linux/include/ -o /var/www/cgi-bin/schemagen-linux/temp/PARSTSTS.o 2>&1 |");

open (IN, "make -d -C schemagen-linux -f makefile 2>&1 |");
while (<IN>) {
    print "<p>"; print <IN>; print "</p>"; 
}

print <<"EndOfHTML";
</BODY>
</HTML>
EndOfHTML


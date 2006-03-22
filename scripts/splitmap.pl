#! /usr/bin/perl

# Kris Parker, 2006

use strict;

if (@ARGV == 0)
{
	print <<EOF;

splitmap.pl
-----------

Break a single map into tiles suitable for a kern-mk-composite-map list

This program is made available under the GPL. Detailed conditions should be found in the file 'COPYING' available with the script.

Note that as currently implemented, the output of the program is a derivitive work of the input data, but NOT a derivitive work of the program itself.

Usage:
splitmap.pl width height input_scm_data

Output is a scm list containing the maps
EOF

exit(0);
}

open (FILEIN,$ARGV[2]) or die "failed to open: $ARGV[1]";

my $mapwidth = $ARGV[0];
my $mapheight = $ARGV[1];

my $mapint = 0;
my @mapbuffer = ();

# check for file header
my $ok = 0;
while (<FILEIN>)
{
	if ($_ =~ /\(list/)
	{
		$ok =1;
		last;
	}
}

($ok) or die "no list found";

sub dumpdata
{
	while (1) # last out later
	{
		my $text;
		print "(kern-mk-map nil $mapwidth $mapheight pal_expanded\n";
		print "\t(list\n";
		for (my $i=0;$i<@mapbuffer;$i++)
		{
			$text = $mapbuffer[$i];
			$mapbuffer[$i]=substr($text,$mapwidth * 3);
			$text=substr($text,0,$mapwidth * 3);
			print "\t\t\"";
			print $text;
			print "\"\n";
		}
		print "\t))\n";
		(length($mapbuffer[0]) > 0) or last;
	}
	@mapbuffer=();
}

while (<FILEIN>)
{
	if ($_ =~ /"(([^"][^"] )+)"/)
	{
		push (@mapbuffer,($1));
		$mapint++;
		if ($mapint == $mapheight)
		{
			$mapint=0;
			dumpdata();
		}
	}
} 
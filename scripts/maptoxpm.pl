#! /usr/bin/perl

# Kris Parker, 2006

if (@ARGV == 0)
{
	print <<EOF;

maptoxpm.pl
-----------

Convert a Nazghul map to an xpm image.

This program is made available under the GPL. Detailed conditions should be found in the file 'COPYING' available with the script.

Note that as currently implemented, the output of the program is a derivitive work of the input data, but NOT a derivitive work of the program itself.

Usage:
maptoxpm.pl palette_data_file input_scm_data

Output is an xpm image suitable for editing in eg The Gimp.

Note that the first list in the input file will be taken to be the map data.

Format for the palette file is:
<2 char map symbol> #<hex colour code>
EOF

exit(0);
}

open (PALIN,$ARGV[0]) or die "failed to open: $ARGV[0]";

my %symbolToColour = ();

while (<PALIN>)
{
	$_ =~ /(..)\s+#(......)/ or die "invalid colourmap line";
	$symbolToColour{$1}=$2;
}
close PALIN;

open (FILEIN,$ARGV[1]) or die "failed to open: $ARGV[1]";

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

my $startoffset = tell(FILEIN);

my $filewidth=0;
my $fileheight=0;

while (<FILEIN>)
{
	if ($_ !~ /"(([^"][^"] )+)"/)
	{
		last;
	}
	$fileheight++;
	if ($filewidth == 0)
	{
		$curline = $1;
		$filewidth = int(length($curline) / 3);
	}
} 

print "/* XPM */\n";
print "static char * map_xpm[] = {\n";
print "\"$filewidth $fileheight ";
my $keycount = keys(%symbolToColour);
print $keycount;
print " 2";

foreach my $key (keys(%symbolToColour))
{
	print "\",\n";
	print "\"$key c #";
	print $symbolToColour{$key};
}

seek (FILEIN,$startoffset,0);

#my $firstline = <FILEIN>;

#my $curline=$firstline;
#my $prevline=$lastline;

while (<FILEIN>)
{
	if ($_ !~ /"(([^"][^"] )+)"/)
	{
		last;
	}
	$curline = $1;
	print "\",\n";
	print "\"";
	while ($curline ne "")
	{
		my $temp = substr($curline,0,2);
		if (!exists($symbolToColour{$temp}))
		{
			print STDERR "No colour found for: $temp\n";
		}	
		print $temp;
		$curline = substr($curline,3);
	}
}
print "\"};";
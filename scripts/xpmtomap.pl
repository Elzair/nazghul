#! /usr/bin/perl

# Kris Parker, 2006

if (@ARGV == 0)
{
	print <<EOF;

xpmtomap.pl
-----------

Convert an xpm image to a Nazghul map.

This program is made available under the GPL. Detailed conditions should be found in the file 'COPYING' available with the script.

Note that as currently implemented, the output of the program is a derivitive work of the input data, but NOT a derivitive work of the program itself.

Usage:
xpmtomap.pl palette_data_file xpm_input_data

Output is a scm list suitable for use as a Nazghul map.

Note that the input xpm file must not have transparency.

Format for the palette file is:
<2 char map symbol> #<hex colour code>
EOF

exit(0);
}

open (FILEIN,$ARGV[0]) or die "failed to open: $ARGV[0]";

my %colourToSymbol = ();

while (<FILEIN>)
{
	$_ =~ /(..)\s+#(......)/ or die "invalid colourmap line";
	$colourToSymbol{$2}=$1;
}
close FILEIN;

open (FILEIN,$ARGV[1]) or die "failed to open: $ARGV[1]";

# check for file header
my $line = <FILEIN>;
if ($line !~ /\/\* XPM \*\//)
{
	die "not an XPM: $ARGV[1]";
}

$line = <FILEIN>;
$line = <FILEIN>;

$line =~ /"(\d+)\s+(\d+)\s+(\d+)\s+(\d+)("|\s.*")/ or die "invalid XPM header";

my $width = $1;
my $height = $2;
my $colours = $3;
my $cellwidth = $4;

print STDERR $width."\n";
print STDERR $height."\n";
print STDERR $colours."\n";
print STDERR $cellwidth."\n";


my %xpmToPal = ();

for (;$colours>0;$colours--)
{
	$line = <FILEIN>;
	$line =~ /"(.)\s+c\s+#(......)",/ or die "invalid colour match line: $line";
	$xpmToPal{$1}=$colourToSymbol{$2};
	print STDERR "$1 => $2 => '$colourToSymbol{$2}'\n";
}

undef %colourToSymbol; #may as well free some memory (we may need it in a bit)

#todo presmooth here?

#my $startoffset = tell(FILEIN);
#
#my $lastline;
#while (<FILEIN>)
#{
#	$lastline = $_;
#}
#
#seek (FILEIN,$startoffset,0);

#my $firstline = <FILEIN>;

#my $curline=$firstline;
#my $prevline=$lastline;

print "(list\n";
while (<FILEIN>)
{
	$_ =~ /"([^"]*)"(,|};)/ or die "invalid line";
	$curline = $1;
	{
		print "  \"";
		while ($curline ne "")
		{
			my $cell = substr($curline,0,$cellwidth);
			$curline = substr($curline,$cellwidth);
			print $xpmToPal{$cell};
			print " ";
		}
		print "\"\n";
	}
}
print ")\n";
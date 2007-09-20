#! /usr/bin/perl

# Kris Parker, 2006

if (@ARGV == 0)
{
	print <<EOF;

mergemap.pl
-----------

Make a single map from a tiled kern-mk-composite-map list

This program is made available under the GPL. Detailed conditions should be found in the file 'COPYING' available with the script.

Note that as currently implemented, the output of the program is a derivitive work of the input data, but NOT a derivitive work of the program itself.

Usage:
mergemap.pl number_submaps_across_merged_map input_scm_data

Output is a scm list containing the combined maps

Note that this will attempt to merge all the maps in the file. If there are additional maps along with the kern-mk-composite-map then the results will be messy.
EOF

exit(0);
}

open (FILEIN,$ARGV[1]) or die "failed to open: $ARGV[1]";

my $mergewidth = $ARGV[0];
my @maps = ();

for (my $i=$mergwidth;$i;$i--)
{
	push (@maps,([]));
}

my $startoffset = tell(FILEIN);

my $filewidth=0;
my $fileheight=0;

my $mapint=0;
my $inmap=0;

print "(list\n";

sub dumpdata
{
	my $len = @{$maps[0]};
	while (@{$maps[0]})
	{
		print "\t\"";
		foreach $map (@maps)
		{
			print(shift(@$map));
		}
		print "\"\n";
	}
}

while (<FILEIN>)
{
	if ($_ =~ /"(([^"][^"] )+)"/)
	{
		$inmap=1;
	}
	if ($inmap)
	{
		if ($_ !~ /"(([^"][^"] )+)"/)
		{
			$inmap=0;
			$mapint++;
			if ($mapint == $mergewidth)
			{
				$mapint=0;
				dumpdata();
			}
		}
		else
		{
			push (@{$maps[$mapint]},($1));
		}
	}
} 

print ")";

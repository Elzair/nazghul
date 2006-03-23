#! /usr/bin/perl

# Kris Parker, 2006

use strict;

if (@ARGV == 0)
{
	print <<EOF;

blendmap.pl
-----------

Preapply blending algorithms to a map

This program is made available under the GPL. Detailed conditions should be found in the file 'COPYING' available with the script.

Note that as currently implemented, the output of the program is a derivitive work of the input data, but NOT a derivitive work of the program itself.

Usage:
blendmap.pl blend_data border_symbol input_scm_data

Output is a scm list of the blended maps from the input

terrain beyond edge of mad is assumed to be of type border_symbol

blend data format:

a symbol is either a 2 character map symbol, or & followed by 2 or more characters, which expands into a list of map symbols 

a symbol list is a set of symbols in brackets ( symbol symbol ... symbol )
(note spaces around brackets (sorry, lazy parser writer))

define a new symbol:
&name symbol_list

define a rule:
symbol_list symbol_list symbol_list symbol_list
representing:
symbols to remap; output symbols to map onto; symbols for superior terrain; symbols for inferior terrain

(if a neighbor is not in either superior or inferior list, no action is taken on the tile)
EOF

exit(0);
}

my $nochange = "{}"; #symbol for no change in sprite. may need to be changed if it gets used.

open (FILEIN,$ARGV[0]) or die "failed to open: $ARGV[1]";

my @transforms;
my %charmaps;

while (<FILEIN>)
{
	if ($_ =~ /^&(\S\S+) \( ((\S\S+ )*)\)$/)
	{
		my $name=$1;
		my $data=$2;
		my $chars=[];
		foreach my $item (split(/ /,$data))
		{
			if ($item =~ /&(\S\S+)/)
			{
				push (@{$chars},(@{$charmaps{$1}})); 
			}
			else
			{
				push (@{$chars},($item));
			}
		}
		$charmaps{$name}=$chars;
		print STDERR "map $name '@{$charmaps{$name}}'\n";
	}
	elsif ($_ =~ /\( ((\S\S+ )*)\) \( ((\S\S+ )*)\) \( ((\S\S+ )*)\) \( ((\S\S+ )*)\)/)
	{
		my @datas=($1,$3,$5,$7);
		my $rules=[];
		for (my $i=0;$i<4;$i++)
		{
			my $data = $datas[$i];
			my $rule = [];
			foreach my $item (split(/ /,$data))
			{
				if ($item =~ /&(\S\S+)/)
				{
					push (@{$rule},(@{$charmaps{$1}})); 
				}
				else
				{
					push (@{$rule},($item));
				}
			}
			$rules->[$i]=$rule;
		}
		print STDERR "rule: \n";
		print STDERR "    @{$rules->[0]}\n";
		print STDERR "    @{$rules->[1]}\n";
		print STDERR "    @{$rules->[2]}\n";
		print STDERR "    @{$rules->[3]}\n";
		push (@transforms,($rules));
	}
}

$ARGV[1] =~ /^(\S\S)$/ or die "invalid border symbol: $ARGV[1]";
my $border = $1;

open (FILEIN,$ARGV[2]) or die "failed to open: $ARGV[2]";

sub getData
{
	my ($data,$loc) = @_;
	defined($data) or return ($border,$border,$border);
	my @ret=($border,$border,$border);
	if ($loc > 0)
	{
		$ret[0] = $data->[$loc-1];
	}
	$ret[1] = $data->[$loc];
	if ($loc < @$data - 1)
	{
		$ret[2] = $data->[$loc+1];
	}
	return @ret;
}

sub inList
{
	my ($list,$char)=@_;
	foreach my $testee (@$list)
	{
		if ($testee eq $char)
		{
			return 1;
		}
	}
	return 0;
}

sub testNeighbor
{
	my ($ruledata,$neighbor,$value,$tally)=@_;
	if (inList($ruledata->[2],$neighbor))
	{
		print STDERR "Y";
		return $value + $tally;
	}
	elsif (inList($ruledata->[3],$neighbor))
	{
		print STDERR "N";
		return $tally;
	}
	print STDERR "?";
	return undef;
}

sub runTest
{
	my ($ruledata,$neighbors,$curtile)=@_;
	if (!inList($ruledata->[0],$neighbors->[4]))
	{
		return $curtile;	
	}
	my $tally=0;
	print STDERR $neighbors->[1];
	$tally = testNeighbor($ruledata,$neighbors->[1],1,$tally);
	defined $tally or return $curtile;
	$tally = testNeighbor($ruledata,$neighbors->[3],2,$tally);
	defined $tally or return $curtile;
	$tally = testNeighbor($ruledata,$neighbors->[5],4,$tally);
	defined $tally or return $curtile;
	$tally = testNeighbor($ruledata,$neighbors->[7],8,$tally);
	defined $tally or return $curtile;
	print STDERR ">> $tally\n";
	my $temptile =  $ruledata->[1]->[$tally];
	defined $temptile or return $curtile;
	if ($temptile eq $nochange)
	{
		return $curtile;
	}
	return $temptile;
}

sub handleLine
{
	my ($lastline,$curline,$nextline) = @_;
	print "\t\"";
	for (my $i=0;$i<@{$curline};$i++)
	{
		my @datalist = (getData($lastline,$i),getData($curline,$i),getData($nextline,$i));
		my $curtile = $datalist[4];
		my $neighbors = \@datalist;
		foreach my $rule (@transforms)
		{
			$curtile = runTest($rule,$neighbors,$curtile);
		}
		print "$curtile ";
	}
	print "\"\n";
}

my $nextline;
my $curline;
my $lastline;
my $inmap=0;

while (<FILEIN>)
{
	if ($inmap)
	{
		if ($_ !~ /"(([^"][^"] )+)"/)
		{
			if (defined($nextline))
			{
				handleLine($curline,$nextline,undef);
			}
			print ")\n\n";
			$inmap=0;
			$lastline = undef;
			$nextline = undef;
			$curline = undef;
		}
		else
		{
			$lastline=$curline;
			$curline=$nextline;
			my @temp = split(/ /,$1);
			$nextline = \@temp;
			if (defined($curline))
			{
				handleLine($lastline,$curline,$nextline);
			}
		}
	}
	elsif ($_ =~ /\(list/)
	{
		print "(list\n";
		$inmap=1;
	}
} 



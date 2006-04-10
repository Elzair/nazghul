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
blendmap.pl blend_data input_scm_data [border_char]

blend_data is a file containing terrain mapping rules.

Output is a scm list of the blended maps from the input.

border_char is char to assume at border areas.
EOF

#
#blend data format:
#
#a symbol is either a 2 character map symbol, or & followed by 2 or more characters, which expands into a list of map symbols 
#
#a symbol list is a set of symbols in brackets ( symbol symbol ... symbol )
#(note spaces around brackets (sorry, lazy parser writer))
#
#define a new symbol:
#&name symbol_list
#
#define a rule:
#rule_name list_of_parameters
#
#list_of_parameters is a set of either a symbol list (as above) or a data list
#
#data list is a set of values (whatever the rule wants) enclosed in square brackets and separated by spaces.
#

exit(0);
}

my $nochange = "{}"; #symbol for no change in sprite. may need to be changed if it gets used.

open (FILEIN,$ARGV[0]) or die "failed to open: $ARGV[1]";

my @transforms;
my %charmaps;
my $mapin;
my $mapout;
my $width;
my $height;

my $border;

if ($ARGV[2] =~ /^(\S\S)$/)
{
	$border = $1;
}

my $offsets=
[
	[0,-1],
	[-1,0],
	[1,0],
	[0,1],
];

sub loadParamList
{
	my $text = $_[0];
	$text =~ s/^ *//;
	$text =~ s/ *$//;
	my $rule = [];
	foreach my $item (split(/ +/,$text))
	{
		push (@{$rule},($item));
	}
	return $rule;
}

sub loadTerrainList
{
	my $text = $_[0];
	$text =~ s/^ *//;
	$text =~ s/ *$//;
	my $rule = [];
	foreach my $item (split(/ +/,$text))
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
	return $rule;
}

sub loadRule
{
	my $text=$_[0];
	my $origtext=$_[0];
	my $rules=[];
	$text =~ s/(\S+)\s// or die "Bad rule definition : $origtext";
	$rules->[0]=$1;
	while ($text ne "")
	{
		if ($text =~ s/\\\s*$//) #continuation marker
		{
			my $temp = <FILEIN>;
			$text .= $temp;
			$origtext .= $temp;
			next;
		}
		if ($text =~ s/^\s+//)
		{
			next;
		}
		if ($text =~ s/^\[( +(([^\]])|(\S\S+) +))\]//)
		{
			push (@{$rules},(loadParamList($1)));
			next;
		}
		if ($text =~ s/^\(( +(\S\S+ +)*)\)//)
		{
			push (@{$rules},(loadTerrainList($1)));
			next;
		}
		die "Bad rule definition : $origtext";
	}
	push (@transforms,($rules));
}

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
		#print STDERR "map $name '@{$charmaps{$name}}'\n";
	}
	elsif ($_ =~ /^\s*#/)
	{
		#ignore it, its a comment
	}
	elsif ($_ =~ /\S/)	
	{
		loadRule($_);
	}
}

open (FILEIN,$ARGV[1]) or die "failed to open: $ARGV[1]";

sub ruleName
{
	$_[0]->[0];
}

sub ruleTargetTerrain
{
	$_[0]->[1];
}

sub ruleOutputTerrain
{
	$_[0]->[2];
}

sub ruleActiveTerrain
{
	$_[0]->[3];
}

sub ruleIgnoreTerrain
{
	$_[0]->[4];
}

sub inList
{
	my ($list,$char)=@_;
	#print "\n$char: \n";
	foreach my $testee (@$list)
	{
		#print "$testee ";
		if ($testee eq $char)
		{
			#print "!\n";
			return 1;
		}
	}
	#print "X\n";
	return 0;
}

sub resetMap
{
	my ($rule,$x,$y)=@_;
	if ($x==0 && $y==0)
	{
		$mapin = $mapout;
		initOut();
	}
}

sub getCell
{
	my ($x,$y)=@_;
	if ($border)
	{
		if ($x<0)
		{
			return $border;
		}
		if ($y<0)
		{
			return $border;
		}
		if ($x >= $width)
		{
			return $border;
		}
		if ($y >= $height)
		{
			return $border;
		}
	}
	else
	{
		while ($x<0)
		{
			$x += $width;
		}
		while ($y<0)
		{
			$y += $height;
		}
		while ($x>=$width)
		{
			$x -= $width;
		}
		while ($y>=$height)
		{
			$y -= $height;
		}
	}
	return $mapin->[$y]->[$x];
}

sub checkNeighbor
{
	my ($rule,$x,$y,$offset)=@_;
	my $xoff = $x + $offset->[0];
	my $yoff = $y + $offset->[1];
	my $cell = getCell($xoff,$yoff);
	if ($cell eq $nochange)
	{
		return undef;
	}
	if (inList(ruleActiveTerrain($rule),$cell))
	{
		return 1;
	}
	elsif (inList(ruleIgnoreTerrain($rule),$cell))
	{
		return 0;
	}
	return undef;
}

sub getTally
{
	my ($rule,$length,$x,$y) = @_;
	#print getCell($x,$y)." >>\n";
	if (!inList(ruleTargetTerrain($rule),getCell($x,$y)))
	{
		return undef;	
	}
	my $tally=0;
	my $tallyinc=1;
	for (my $neighbor = 0; $neighbor < $length; $neighbor++)
	{
		my $cellValue = checkNeighbor($rule,$x,$y,$offsets->[$neighbor]);
		defined ($cellValue) or return undef;
		if ($cellValue)
		{
			$tally += $tallyinc;
		}
		$tallyinc *= 2;
	}
	#print STDERR $tally."\n";
	return $tally
}

sub writeMap
{
	my ($x,$y,$cell)= @_;
	defined $cell or return;
	if ($cell eq $nochange)
	{
		return;
	}
	$mapout->[$y]->[$x]=$cell;
}

sub baseCheck
{
	my ($rule,$x,$y) = @_;
	my $length = 4;
	my $tally = getTally($rule,$length,$x,$y);
	defined ($tally) or return;
	writeMap($x, $y, ruleOutputTerrain($rule)->[$tally]);
}

#sub farCheck
#{
#	my ($rule,$x,$y)= @_;
#	my $length = ruleParam($rule);
#	my $tally = getTally($rule,$length,$x,$y);
#	defined ($tally) or return;
#	my $outTer = ruleOutputTerrain($rule);
#	my $outChar;
#	for (my $i = 0; $i<@($outTer);$i+=3)
#	{
#		my $tempTally = $tally & $outTer->[$i];
#		if ($tempTally == $outTer->[$i+1])
#		{
#			$outChar = $outTer->[$i+2]
#		}
#	}
#}

#########################################

sub checkCell
{
	my ($activeList,$ignoreList,$x,$y)=@_;
	my $cell = getCell($x,$y);
	if ($cell eq $nochange)
	{
		return undef;
	}	
	if (inList($activeList,$cell))
	{
		return 1;
	}
	elsif (inList($ignoreList,$cell))
	{
		return 0;
	}
	return undef;	
}

sub checkCornerCell
{
	my ($rule,$xc,$yc,$xo,$yo)=@_;
	my $cornerList = $rule->[3]; 
	my $edgeList = $rule->[4];
	my $ignoreList = $rule->[5];
	my $xoff = $xc + $xo;
	my $yoff = $yc + $yo;
	my $match = 1;
	my $cellret;
	$cellret = checkCell($cornerList,$ignoreList,$xoff,$yoff);
	defined $cellret or return undef;
	$match &= $cellret;
	$cellret = checkCell($edgeList,$ignoreList,$xoff,$yc);
	defined $cellret or return undef;
	$match &= $cellret;
	$cellret = checkCell($edgeList,$ignoreList,$xc,$yoff);
	defined $cellret or return undef;
	$match &= $cellret;
	return $match;
}

sub getCornerTally
{
	my ($rule,$x,$y) = @_;
	my $cornerOffsets=
	[
		[1+2,-1,-1],
		[1+4,1,-1],
		[8+2,-1,1],
		[8+4,1,1]
	];
	if (!inList(ruleTargetTerrain($rule),getCell($x,$y)))
	{
		return undef;	
	}
	my $tally=0;
	#print getCell($x,$y)." >>\n";
	for (my $neighbor = 0; $neighbor < 4; $neighbor++)
	{
		my ($tallyinc,$xoff,$yoff)=@{$cornerOffsets->[$neighbor]};
		#print "    $tallyinc ($x $y) ($xoff $yoff) ";
		my $cellValue = checkCornerCell($rule,$x,$y,$xoff,$yoff);
		#print "= $cellValue \n";
		defined ($cellValue) or return undef;
		if ($cellValue)
		{
			$tally |= $tallyinc;
		}
	}
	#print $tally."\n";
	return $tally
};

sub cornerCheck
{
	my ($rule,$x,$y)=@_;
	my $tally = getCornerTally($rule,$x,$y);
	defined ($tally) or return;
	writeMap($x,$y,ruleOutputTerrain($rule)->[$tally]);
}

##########################################

sub checkEdgeCell
{
	my ($rule,$xc,$yc,$xe,$ye)=@_;
	my $cornerList = $rule->[3]; 
	my $edgeList = $rule->[4];
	my $ignoreList = $rule->[5];
	my $match = 1;
	my $cellret;
	#print "\n\n";
	my $xem = $xc - $xe;
	my $yem = $yc - $ye;
	my $xep = $xc + $xe;
	my $yep = $yc + $ye;
	#print "\n ($xem $yem) ($xc $yc) ($xep $yep)";
	#print " (".$xc." ".$yc.") ";
	#print " (".$xc+$xe." ".$yc+$ye.") <<?\n";
	$cellret = checkCell($cornerList,$ignoreList,$xc-$xe,$yc-$ye);
	defined $cellret or return undef;
	$match &= $cellret;
	$cellret = checkCell($edgeList,$ignoreList,$xc,$yc);
	defined $cellret or return undef;
	$match &= $cellret;
	$cellret = checkCell($cornerList,$ignoreList,$xc+$xe,$yc+$ye);
	defined $cellret or return undef;
	$match &= $cellret;
	return $match;
}

sub getEdgeTally
{
	my ($rule,$x,$y) = @_;
	my $edgeOffsets=
	[
		[1,0,-1],
		[2,-1,0],
		[4,1,0],
		[8,0,1]
	];
	if (!inList(ruleTargetTerrain($rule),getCell($x,$y)))
	{
		return undef;	
	}
	my $tally=0;
	#print getCell($x,$y)." >>\n";
	for (my $neighbor = 0; $neighbor < 4; $neighbor++)
	{
		my ($tallyinc,$xoff,$yoff)=@{$edgeOffsets->[$neighbor]};
		my $xedge = 0;
		my $yedge= 0;
		if ($xoff == 0)
		{
			$xedge = 1;
		}
		if ($yoff == 0)
		{
			$yedge=1;
		}
		#print "    $tallyinc ($x $y) ($xoff $yoff) ";
		my $cellValue = checkEdgeCell($rule,$x+$xoff,$y+$yoff,$xedge,$yedge);
		#print "= $cellValue \n";
		defined ($cellValue) or return undef;
		if ($cellValue)
		{
			$tally |= $tallyinc;
		}
	}
	#print $tally."\n";
	return $tally
};

sub edgeCheck
{
	my ($rule,$x,$y) = @_;
	my $tally = getEdgeTally($rule,$x,$y);
	defined ($tally) or return;
	writeMap($x,$y,ruleOutputTerrain($rule)->[$tally]);
}

###########################################

sub replaceOp
{
	my ($rule,$x,$y)=@_;
	if (!inList(ruleTargetTerrain($rule),getCell($x,$y)))
	{
		return undef;	
	}
	writeMap($x,$y,ruleOutputTerrain($rule)->[0]);
}

sub randomOp
{
	my ($rule,$x,$y)=@_;
	if (!inList(ruleTargetTerrain($rule),getCell($x,$y)))
	{
		return undef;	
	}
	my $randno=rand();
	my $problist = $rule->[2];
	my $i;
	my $outputs = $rule->[3];
	for ($i=0;$i<@{$problist};$i++)
	{
		if ($randno < $problist->[$i])
		{
			last;
		}
	}
	writeMap($x,$y,$outputs->[$i]);
}

########################################

my %ruleTypes=
(
	"basic"=>\&baseCheck,
	"corner"=>\&cornerCheck,
	"replace" => \&replaceOp,
	"reset"=>\&resetMap,
	"random"=>\&randomOp,
	"edge"=>\&edgeCheck
);

sub checkRule
{
	my ($rule,$x,$y) =@_;
	my $name = ruleName($rule);
	&{$ruleTypes{$name}}($rule,$x,$y);
}

sub handleMap
{
	# rule first to allow map reset
	foreach my $rule (@transforms)
	{
		print STDERR ".";
		for (my $x=0;$x<$width;$x++)
		{
			for (my $y=0;$y<$height;$y++)
			{
				checkRule($rule,$x,$y);
			}
		}
	}
}

sub initOut
{
	$mapout = [];
	foreach my $line (@{$mapin})
	{
		my $temp = [];
		push (@{$temp},(@{$line}));
		push (@{$mapout},($temp));
	}
}

sub printMap
{
	print "\t(list\n";
	foreach my $line (@{$mapout})
	{
		print "\t\t\"";
		foreach my $cell (@{$line})
		{
			print "$cell ";
		}
		print "\"\n";
	}
	print "\t)\n\n";
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
			if (@{$mapin} > 0)
			{
				$width=@{$mapin->[0]};
				$height=@{$mapin};
				initOut();
				handleMap();
				printMap();

			}
			$inmap=0;
		}
		else
		{
			my @temp = split(/ /,$1);
			push (@{$mapin},(\@temp));
		}
	}
	elsif ($_ =~ /\(list/)
	{

		$mapin=[];
		$inmap=1;
	}
} 



#!/usr/bin/perl

# Quick n nasty CVS log dump to Changelog reformatter
# May need to be adapted, esp wrt the authors list

my $cvsrootlength = 25; # remove initial <n> characters of filename to reach actual files instead of CVS bumf

# cvs username to author name table
my %authorlist =
(
"baueran"=>"Andreas Bauer  <baueran\@localhost.localdomain>",
"gmcnutt"=>"Gordon McNutt  <gmcnutt\@localhost.localdomain>",
"icepic"=>"Janne Johansson  <icepic\@localhost.localdomain>",
"kaypy"=>"Kris Parker  <kaypy\@localhost.localdomain>",
"sglasby"=>"Sam Glasby  <sglasby\@localhost.localdomain>",
"timdoug"=>"Tim Douglas  <timdoug\@localhost.localdomain>",
"wolfoxbr"=>"Roberto Amorim  <wolfoxbr\@localhost.localdomain>",
);


my $state=0;
my $file;
my $date;
my $time;
my $author;
my $data;
my @datasort=();

sub handleData
{
	defined($file) or return;
	defined($date) or return;
	defined($time) or return;
	defined($author) or return;
	defined($data) or return;
	my $i=0;
	my $match = 0;
	for ($i=0;$i<@datasort;$i++)
	{
		my $ref = $datasort[$i];
		if ($date lt $ref->[0])
		{
			next;
		}
		elsif ($date gt $ref->[0])
		{
			last;
		}
		if ($author lt $ref->[3])
		{
			next;
		}
		elsif ($author gt $ref->[3])
		{
			last;
		}
		if ($data ne $ref->[4])
		{
			next;
		}
		$match = 1;
		last;
	}
	if ($i >= @datasort)
	{
		push @datasort, ([$date,[substr($file,$cvsrootlength)],$time,$author,$data]);
	}
	elsif ($match)
	{
		push @{$datasort[$i]->[1]},(substr($file,$cvsrootlength));
	}
	else
	{
		splice @datasort, $i, 0,([$date,[substr($file,$cvsrootlength)],$time,$author,$data]);
	}
}

while($line = <STDIN>)
{
	if ($state==0)
	{
		if ($line =~ /^RCS file: (.*),v$/)
		{
			$file = $1;
		}
		elsif ($line =~ /^date: (\d\d\d\d)\/(\d?\d)\/(\d?\d) (.*);  author: (.*);  state: /)
		{
			my $yr = $1;
			my $mon = $2;
			my $day = $3;
			$time = $4;
			#if ($mon < 10)
			#{
			#	$mon = "0$mon";
			#}
			#if ($day < 10)
			#{
			#	$day = "0$day";
			#}
			$date = "$yr-$mon-$day";
			$author = $5;			
			$state = 1;
			$data = "";
		}
	}
	if ($line =~ /^----------------------------$/)
	{
		handleData();
		$state = 0;
		undef $time;
		undef $author;
		undef $data;
	}
	elsif ($line =~ /^=============================================================================$/)
	{
		handleData();
		$state = 0;
		undef $time;
		undef $author;
		undef $file;
		undef $data;
	}	
	elsif ($line =~ /branches: /)
	{
	
	}
	elsif ($state == 1)
	{
		$state = 2;
	}
	elsif ($state == 2)
	{
		$data .= $line;
	}
}

my $i;
my $lastdate = "";
my $lastauthor = "";
for ($i=0;$i<@datasort;$i++)
{
	my $ref = $datasort[$i];
	if ($lastdate ne $ref->[0] || $lastauthor ne $ref->[3])
	{
		print "\n";
		print $ref->[0];
		print "  ";
		exists ($authorlist{$ref->[3]}) or print stderr "No author found for $ref->[3]";
		print $authorlist{$ref->[3]};
		print "\n";
		$lastdate = $ref->[0];
		$lastauthor = $ref->[3];
	}
	print "\n";
	print "\t* ";
	print join ", ", @{$ref->[1]};
	print ":\n\t";
	$datatext = $ref->[4];
	$datatext =~ s/\n$//;
	$datatext =~ s/\n/\n\t/g;
	$datatext =~ s/\t$//;
	print $datatext;
	print "\n";
}
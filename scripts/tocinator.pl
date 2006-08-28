#! /usr/bin/perl -w 

my @levels = ();
my $index = 1;
my $grabtitle="";
my $curlevel = 1;

open(TOCOUT,"> TOCOUTPUT.txt") or die "failed on output";

while ($line = <STDIN>)
{
	if ($line =~ /<h(\d)>/)
	{
		my $newlevel = $1;
		print STDERR "$newlevel ($curlevel)\n";		
		while ($newlevel < $curlevel)
		{
			print TOCOUT "\t" x (2 * $curlevel + 1);
			print TOCOUT "</li>\n";
			print TOCOUT "\t" x (2 * $curlevel);
			print TOCOUT "</ul>\n";		
			$curlevel = pop(@levels);
			print STDERR "        <- $curlevel\n";
		}
		if ($newlevel == $curlevel)
		{
			print TOCOUT "\t" x (2 * $curlevel + 1);
			print TOCOUT "</li>\n";
		}
		elsif ($newlevel > $curlevel)
		{
			print STDERR "$newlevel -> $curlevel\n";
			print TOCOUT "\t" x (2 * $newlevel);
			print TOCOUT "<ul>\n";
			push @levels,($curlevel);
			$curlevel = $newlevel;
		}
		print TOCOUT "\t" x (2 * $curlevel + 1);
		print TOCOUT "<li>\n";
		print TOCOUT "\t" x (2 * $curlevel + 2);
		print TOCOUT '<a href="#toc'.$index.'">';
		print '<a name="toc'.$index.'"></a>'."\n";
		$grabtitle = $line;
		$index++;
		print $line;
		next;
	}
	if ($grabtitle)
	{
		$title=$line;
		chomp $title;
		$title =~ s/^\s+//;
		$title =~ s/\s+$//;
		print TOCOUT "$title</a>\n";
		$grabtitle = "";
	}
	if ($line !~ /<a name="toc/)
	{
		print $line;
	}
}

while ($curlevel = pop(@levels))
{
	print TOCOUT "\t" x (2 * $curlevel + 1);
	print TOCOUT "</li>\n";
	print TOCOUT "\t" x (2 * $curlevel);
	print TOCOUT "</ul>\n";		
}

close TOCOUT;
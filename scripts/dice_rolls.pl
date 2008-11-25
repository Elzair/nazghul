#!/usr/bin/perl -w

$#ARGV==1 || die "Usage: dice_rolls.pl <faces 1> <faces 2>";

my $D1=shift @ARGV;
my $D2=shift @ARGV;
my %COUNT;
my $total=0;

for ($d1=1;$d1<=$D1;$d1++){
    for($d2=1;$d2<=$D2;$d2++){
        $COUNT{$d1+$d2} += 1;
        $total+=1;
    }
}

my $cum_percent=0;
foreach $roll(sort {$a <=> $b} (keys(%COUNT))) {
    $val=$COUNT{$roll};
    $percent = ($val*100) / $total;
    $cum_percent += $percent;
    printf("%d,%4f%%\n", $roll, $cum_percent);
}

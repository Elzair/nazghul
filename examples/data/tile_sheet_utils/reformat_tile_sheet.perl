#!/usr/bin/perl -w

# SAM:
# This program was used to reformat a 1x256 strip
# of 32x32 tiles into a 16x16 grid of 32x32 tiles
# by loading the file as a TileSheet, displaying
# them 16 across, and then capturing a screenshot
# with the Gimp.

# TODO:  (partially done)
# Currently, I edit values in the script for most parameters,
# for each new tilesheet-->tilesheet conversion.
# I should spend 5 minutes and parse args for all the bits
# which may vary.  There would be a BEFORE and an AFTER value
# for each of:
#     $num_tiles,      $tiles_across,
#     $tile_size_X,    $tile_size_Y,
#     $offset_X,       $offset_Y,
#     $intra_X,        $intra_Y
# 


use SDL::App;
use SDL::Surface;
use SDL::Color;
use SDL::Event;

use TileSheet;

unless (scalar @ARGV == 3)
{
    die "Usage:  $0 <in_file> <num_tiles> <tiles_across>\n";
}
### Original sheet attributes:
my $file         = $ARGV[0];
my $num_tiles    = $ARGV[1];
my $tiles_across = $ARGV[2];

my ($tile_width, $tile_height) = (8,8); # (32,32) or (8,16) most common
my ($offset_X,   $offset_Y)    = (0,0);   # (0,0) or (1,1) most common
my ($intra_X,    $intra_Y)     = (0,0);   # (0,0) or (1,1) most common

### New sheet attributes:
my $new_across = 16;
my ($new_base_x,  $new_base_y)  = (0,0);  # (0,0) or (1,1) most common
my ($new_intra_X, $new_intra_Y) = (0,0);  # (0,0) or (1,1) most common


### Load the TileSheet into memory:
my $sheet = TileSheet->new("tilesheet", $file,
                           $num_tiles,  $tiles_across,
                           $tile_width, $tile_height,
                           $offset_X,   $offset_Y,
                           $intra_X,    $intra_Y);

### TODO: Better to make the size fit what is needed...
my $app_window = SDL::App->new(
                           -width  => 800, 
                           -height => 600,
                           );
my $surf = $sheet->surface();

### First, a pink background: (for any remaining intra space)
my $pink = SDL::Color->new('-r' => 255, '-g' => 0, '-b' => 255);
my $entire_rect = SDL::Rect->new('-width' => 800, '-height' => 600);
$app_window->fill(0, $pink);
$app_window->update($entire_rect);


### Blit the TileSheet in the new arrangement:
my ($x,$y) = ($new_base_x,$new_base_y);
my $width  = $sheet->tile_size_X();
my $height = $sheet->tile_size_Y();

foreach my $tile (1..$num_tiles)
{
    my $tile_rect = $sheet->rect_for_tile($tile);
    my $dest_rect = SDL::Rect->new('-x' => $x, '-y' => $y);
    
    $surf->blit($tile_rect, $app_window, $dest_rect);  
    $app_window->update($dest_rect);

    if ($tile % $new_across)
    {
        # Next tile over on same row
        $x += ($width + $new_intra_X);
    }
    else
    {
        # Start new row on left hand side
        $x = $new_base_x;
        $y += ($height + $new_intra_Y);
    }
} # foreach ($tile)


### Loop & process the events:
my $events_hashref = {
    SDL_KEYDOWN() => sub { exit(); },
    SDL_QUIT()    => sub { exit(); },
};
$app_window->loop($events_hashref);

exit 0;

# eof

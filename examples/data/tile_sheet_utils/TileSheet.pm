#!/usr/bin/perl -w
# 
# TileSheet
#     Defines a TileSheet type representing a large image
#     with many tiles of the same size arranged in a regular pattern.
# 
###########################################################################
# 
# BADARG(@arg_indexes)
# 
# $TileSheet_ref = new ($tilesheet_name, $image_filename,
#                       $num_tiles,      $tiles_across,
#                       $tile_size_X,    $tile_size_Y,
#                       $offset_X,       $offset_Y,
#                       $intra_X,        $intra_Y)
# 
# $TileSheet_ref = newFromFile ($filename)
# 
# $ = name()
# $ = filename()
# $SDL::Surface_ref = surface()
# 
# $ = num_tiles()
# $ = tiles_across()
# 
# $ = offset_X()
# $ = offset_Y()
# 
# $ = intra_X()
# $ = intra_Y()
# 
# $ = tile_size_X()
# $ = tile_size_Y()
# 
# $ = valid_tile_number ($tile_num)
# @ = tile_XY_for_tile  ($tile_num)
# @ = XYWH_for_tile     ($tile_num)
# $SDL::Rect_ref = rect_for_tile ($tile_num)
#     blit_tile ($tile_num, $dest_surface, $x, $y, $update)
# 
# 
###########################################################################
# 
# Below is depicted a small tile sheet for illustration.
# The TileSheet attributes (other than name and filename) 
# for this example are:
# 
#       num_tiles = 7,  tiles_across = 4
#     tile_size_X = 10,  tile_size_Y = 12
#        offset_X = 3,      offset_Y = 1
#         intra_X = 2,       intra_Y = 1
# 
# This is the example TileSheet considered as tiles.
# It is from this viewpoint that tile_XY_for_tile($tile_num)
# reports, returning (1,0) for tile number 6:
# 
#     1  2  3  4
#     5  6  7  
# 
# Below can be seen tiles 1..7 of the 4x3 tile sheet above.
# 
# 1) Notice the unused space on the left (offset_X) and the top (offset_Y).
#    Some tile sheets will have offset_X = 0 and/or offset_Y = 0.
# 
# 2) Notice the space between tiles left-right (intra_X) 
#                              and above-below (intra_Y).
#    Some tile sheets will have intra_X = 0 and/or intra_Y = 0.
# 
# 3) Notice the unused space rightwards and below the numbered tiles.
#    Some tile sheets will have no unused space to the right / below.
#    Note also that the last line of tiles may be incomplete;
#    the tile sheet below has no tile 8.
# 
# ........................................................
# ...1111111111..2222222222..3333333333..4444444444.......
# ...1111111111..2222222222..3333333333..4444444444.......
# ...1111111111..2222222222..3333333333..4444444444.......
# ...1111111111..2222222222..3333333333..4444444444.......
# ...1111111111..2222222222..3333333333..4444444444.......
# ...1111111111..2222222222..3333333333..4444444444.......
# ...1111111111..2222222222..3333333333..4444444444.......
# ...1111111111..2222222222..3333333333..4444444444.......
# ...1111111111..2222222222..3333333333..4444444444.......
# ...1111111111..2222222222..3333333333..4444444444.......
# ...1111111111..2222222222..3333333333..4444444444.......
# ...1111111111..2222222222..3333333333..4444444444.......
# ........................................................
# ...5555555555..6666666666..7777777777...................
# ...5555555555..6666666666..7777777777...................
# ...5555555555..6666666666..7777777777...................
# ...5555555555..6666666666..7777777777...................
# ...5555555555..6666666666..7777777777...................
# ...5555555555..6666666666..7777777777...................
# ...5555555555..6666666666..7777777777...................
# ...5555555555..6666666666..7777777777...................
# ...5555555555..6666666666..7777777777...................
# ...5555555555..6666666666..7777777777...................
# ...5555555555..6666666666..7777777777...................
# ...5555555555..6666666666..7777777777...................
# ........................................................
# ........................................................
# 
###########################################################################

package TileSheet;

use FileHandle;

use SDL::Surface;
use SDL::Rect;


sub BADARG {
    my ($p_name, $f_full_path, $line, $sub_name, 
        $args, $want_array) = caller(1);
    die("Function $sub_name, Line $line got bad args.\n",
        "    Arg list = (", join(", ", $args), ")\n",
        "    The args with these indexes were bad: (", join(",", @_), ")");
} # BADARG()


sub new {
    my ($caller, 
        $tilesheet_name, $image_filename,
        $num_tiles,      $tiles_across,
        $tile_size_X,    $tile_size_Y,
        $offset_X,       $offset_Y,
        $intra_X,        $intra_Y,  ) = @_;
    my $class = ref($caller) || $caller;

    # Some arg checking:
    BADARG(2)   unless ($tilesheet_name);
    BADARG(3)   unless (-r $image_filename);

    my $surface = SDL::Surface->new('-name' => $image_filename);
    unless (ref $surface)
    {
        die("TileSheet::new() failed to load SDL_Surface \n",
            "    from file '$image_filename' because '$!'.\n");
    }

    BADARG(4,5) unless ($num_tiles >= $tiles_across);

    BADARG(6)   unless ($tile_size_X > 0);
    BADARG(7)   unless ($tile_size_Y > 0);

    BADARG(8)   unless ($offset_X >= 0);
    BADARG(9)   unless ($offset_Y >= 0);

    BADARG(10)  unless ($intra_X >= 0);
    BADARG(11)  unless ($intra_Y >= 0);

    my $width         = $surface->width();
    my $claimed_width = ($offset_X + 
                         ($tile_size_X * $tiles_across) + 
                         ($intra_X     * ($tiles_across - 1)) );
    BADARG(5,6,8,10) unless ($width >= $claimed_width);

    my $height         = $surface->height();
    my $tiles_down     = int($num_tiles / $tiles_across);
    my $claimed_height = ($offset_Y + 
                          ($tile_size_Y * $tiles_down) + 
                          ($intra_Y     * ($tiles_down - 1)) );
    BADARG(4,5,7,9,11) unless ($height >= $claimed_height);

    # Construct the object:
    my $self = {
        "name"         => $tilesheet_name,
        "filename"     => $image_filename,
        "surface"      => $surface,

        "num_tiles"    => $num_tiles,
        "tiles_across" => $tiles_across,

        "tile_size_X"  => $tile_size_X,
        "tile_size_Y"  => $tile_size_Y,

        "offset_X"     => $offset_X,
        "offset_Y"     => $offset_Y,

        "intra_X"      => $intra_X,
        "intra_Y"      => $intra_Y,
    };
    bless($self, $class);

    # print("TileSheet->new() self = $self\n",
    #       "name         = ", $self->name(), "\n",
    #       "filename     = ", $self->filename(), "\n",
    #       "surface      = ", $self->surface(), "\n",
    #       "num_tiles    = ", $self->num_tiles(), "\n",
    #       "tiles_across = ", $self->tiles_across(), "\n",
    #       "tile_size_X  = ", $self->tile_size_X(), "\n",
    #       "tile_size_Y  = ", $self->tile_size_Y(), "\n",
    #       "offset_X     = ", $self->offset_X(), "\n",
    #       "offset_Y     = ", $self->offset_Y(), "\n",
    #       "intra_X      = ", $self->intra_X(), "\n",
    #       "intra_Y      = ", $self->intra_Y(), "\n",
    #       "\n" );

    return $self;
} # new()


sub newFromFile {
    my ($caller, $filename) = @_;
    my $class = ref($caller) || $caller;

    BADARG(1) unless (-r $filename);
    my $fh = FileHandle->new($filename, "<");
    unless (ref $fh)
    {
        die("TileSheet::newFromFile() failed to load tile sheet description\n",
            "    from file '$filename' because '$!'.\n");
    }
    my @lines = $fh->getlines();
    $fh->close();

    my ($tilesheet_name, $image_filename,
        $num_tiles,      $tiles_across,
        $tile_size_X,    $tile_size_Y,
        $offset_X,       $offset_Y,
        $intra_X,        $intra_Y,
        );
    my $n = 0;
    foreach my $line (@lines)
    {
        $n++;
        $line =~ m/^\s*$/  && next;  # Skip   blank lines
        $line =~ m/^\s*\#/ && next;  # Skip comment lines

        if ( $line =~ m/^\s*(\w+)\s*=\s*\'([\w\/\.]+)\'\s*$/ || 
             $line =~ m/^\s*(\w+)\s*=\s*\"([\w\/\.]+)\"\s*$/ )
        {
            # foo = 'bar'  foo = 'bar/baz.gif'
            # foo = "bar"  foo = "bar/baz.gif"
            my $key   = $1;
            my $value = $2;
            
            $key =~ m/^name$/i         && ($tilesheet_name = $value);
            $key =~ m/^filename$/i     && ($image_filename = $value);

            next;  # Done with this line
        }
        
        if ( $line =~ m/^\s*(\w+)\s*=\s*(\d+)\s*$/ )
        {
            # foo = 123
            my $key   = $1;
            my $value = $2;

            $key =~ m/^num_tiles$/i    && ($num_tiles      = $value);
            $key =~ m/^tiles_across$/i && ($tiles_across   = $value);
            $key =~ m/^tile_size_x$/i  && ($tile_size_X    = $value);
            $key =~ m/^tile_size_y$/i  && ($tile_size_Y    = $value);

            $key =~ m/^offset_x$/i     && ($offset_X       = $value);
            $key =~ m/^offset_y$/i     && ($offset_Y       = $value);
            $key =~ m/^intra_x$/i      && ($intra_X        = $value);
            $key =~ m/^intra_y$/i      && ($intra_Y        = $value);

            next;  # Done with this line
        }

        print("TileSheet::newFromFile() --\n",
              "    Parsing line $n of file '$filename', \n",
              "    found strange contents '$line'.\n");
    } # foreach ($line)

    # Some (minimal) verification of valid values:
    defined ($tilesheet_name) || print "Missing tilesheet_name.\n";
    defined ($image_filename) || print "Missing image_filename.\n";

    defined ($num_tiles)      || print "Missing num_tiles.\n";
    defined ($tiles_across)   || print "Missing tiles_across.\n";
    defined ($tile_size_X)    || print "Missing tile_size_X.\n";
    defined ($tile_size_Y)    || print "Missing tile_size_Y.\n";

    defined ($offset_X)       || print "Missing offset_X.\n";
    defined ($offset_Y)       || print "Missing offset_Y.\n";
    defined ($intra_X)        || print "Missing intra_X.\n";
    defined ($intra_Y)        || print "Missing intra_Y.\n";

    my $self = $class->new($tilesheet_name, $image_filename,
                           $num_tiles,      $tiles_across,
                           $tile_size_X,    $tile_size_Y,
                           $offset_X,       $offset_Y,
                           $intra_X,        $intra_Y,
                           );
    return $self;
} # newFromFile()


###########################################################################


sub name         { my ($self) = @_; return $self->{name}         }
sub filename     { my ($self) = @_; return $self->{filename}     }
sub surface      { my ($self) = @_; return $self->{surface}      }

sub num_tiles    { my ($self) = @_; return $self->{num_tiles}    }
sub tiles_across { my ($self) = @_; return $self->{tiles_across} }

sub offset_X     { my ($self) = @_; return $self->{offset_X}     }
sub offset_Y     { my ($self) = @_; return $self->{offset_Y}     }

sub intra_X      { my ($self) = @_; return $self->{intra_X}      }
sub intra_Y      { my ($self) = @_; return $self->{intra_Y}      }

sub tile_size_X  { my ($self) = @_; return $self->{tile_size_X}  }
sub tile_size_Y  { my ($self) = @_; return $self->{tile_size_Y}  }


###########################################################################


sub valid_tile_number {
    # Given $num, return either a "tile number"
    # (an integer index in the range [1..$num_tiles] 
    # into the TileSheet $self),
    # or zero for an invalid index.
    my ($self, $num) = @_;

    my $num_tiles = $self->num_tiles();
    unless ($num =~ m/^\d+$/ &&
            $num >= 1 && 
            $num <= $num_tiles) 
    {
        return 0;  # Invalid tile number
    }
    return $num;  # Valid tile number
} # valid_tile_number()


sub tile_XY_for_tile {
    # Given $tile_num (1-based), 
    # return offsets x,y in tiles (zero-based)
    # in the tile sheet $self.
    my ($self, $tile_num) = @_;
    unless ( $self->valid_tile_number($tile_num) )
    {
        return undef;
    }
    my $tiles_across = $self->tiles_across();

    # $tile_X and $tile_y are zero-based, while tile_num is 1-based.
    my $tile_X =    ( ($tile_num - 1) % $tiles_across);
    my $tile_Y = int( ($tile_num - 1) / $tiles_across);

    # print "tile $tile_num,  TX,TY=($tile_X,$tile_Y)\n";

    return ($tile_X, $tile_Y);
} # tile_XY_for_tile()


sub XYWH_for_tile {
    my ($self, $tile_num) = @_;

    my ($tile_X, $tile_Y) = $self->tile_XY_for_tile($tile_num);

    my $tile_width  = $self->tile_size_X();
    my $tile_height = $self->tile_size_Y();
    
    my $intra_X     = $self->intra_X();
    my $intra_Y     = $self->intra_Y();

    my $offset_X    = $self->offset_X();
    my $offset_Y    = $self->offset_Y();

    my $x = $tile_X * ($tile_width  + $intra_X) + $offset_X;
    my $y = $tile_Y * ($tile_height + $intra_Y) + $offset_Y;

    return ($x,$y, $tile_width,$tile_height);
} # XYWH_for_tile()


sub rect_for_tile {
    my ($self, $tile_num) = @_;
    my ($x,$y, $w,$h) = $self->XYWH_for_tile($tile_num);

    my $rect = SDL::Rect->new();
    $rect->x($x);
    $rect->y($y);
    $rect->width($w);
    $rect->height($h);

    # print("Tile $tile_num, X,Y=($x,$y) W,H=($w,$h)\n");

    return $rect;
} # rect_for_tile()


sub blit_tile {
    # Blit the tile corresponding to $tile_num
    # onto $dest_surface at $x,$y, and update that surface if called for.
    my ($self, $tile_num, $dest_surface, $x, $y, $update) = @_;

    my $surface   = $self->surface();
    my $tile_rect = $self->rect_for_tile($tile_num);
    my $dest_rect = SDL::Rect->new('-x' => $x, '-y' => $y);

    $surface->blit($tile_rect, $dest_surface, $dest_rect);  
    if ($update)
    {
        $dest_surface->update($dest_rect);
    }
} # blit_tile()


###########################################################################

1;  # Successful package loading.

# eof

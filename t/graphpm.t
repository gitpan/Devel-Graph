#!/usr/bin/perl -w

use Test::More;
use strict;

BEGIN
   {
   plan tests => 7;
   chdir 't' if -d 't';
   use lib '../lib';
   use_ok ("Devel::Graph") or die($@);
   };

#############################################################################

can_ok ('Devel::Graph',
  qw/
    new
    graph
    first_block
    last_block
    current_block

    new_block

    add_block
    add_joint
    add_if_then
    add_if_then_else
    add_for
    add_while
  /);

#############################################################################
# graph() interface

#my $graph = Devel::Graph->graph( '$a = 9;' );
#
#is (ref($graph), 'Graph::Easy');
#
#is ($graph->error(), '', 'no error yet');
#
#is ($graph->nodes(), 1, '1 node');

#############################################################################
# OO interface

my $grapher = Devel::Graph->new();

my $first = $grapher->first_block();
my $last = $grapher->first_block();
my $curr = $grapher->current_block();

is (ref($first), 'Devel::Graph::Node');
is (ref($last), 'Devel::Graph::Node');
is (ref($curr), 'Devel::Graph::Node');

is ($curr, $last, 'last and curr are the same');
is ($curr, $first, 'first and curr are the same');

#$grapher->decompose( '$a = 9;' );



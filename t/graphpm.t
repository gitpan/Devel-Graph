#!/usr/bin/perl -w

use Test::More;
use strict;

BEGIN
   {
   plan tests => 8;
   chdir 't' if -d 't';
   use lib '../lib';
   use_ok ("Devel::Graph") or die($@);
   };

#############################################################################

can_ok ('Devel::Graph',
  qw/
    new
    graph
    decompose
    reset
  /);

#############################################################################
# graph() interface

my $graph = Devel::Graph->graph( \'$a = 9;' );

is (ref($graph), 'Graph::Easy');

is ($graph->error(), '', 'no error yet');
is ($graph->nodes(), 3, '3 nodes');

#############################################################################
# OO interface

my $grapher = Devel::Graph->new();

is (ref($grapher), 'Devel::Graph');

$graph = $grapher->graph( \'$a = 9;' );

is (ref($graph), 'Graph::Easy', 'graph()');

my $flow = $grapher->decompose( \'$a = 9;' );

is (ref($flow), 'Devel::Graph', 'decompose()');


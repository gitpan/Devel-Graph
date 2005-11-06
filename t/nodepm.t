#!/usr/bin/perl -w

use Test::More;
use strict;

BEGIN
   {
   plan tests => 8;
   chdir 't' if -d 't';
   use lib '../lib';
   use_ok ("Devel::Graph::Node") or die($@);
   };

#############################################################################

can_ok ('Devel::Graph::Node',
  qw/
    new
  /);

my $node = Devel::Graph::Node->new ( '$a = 0;' );
is ($node->{_type}, 1, 'type got set');
is ($node->name(), "0", 'name got set');
is ($node->label(), '$a = 0;', 'label');

$node = Devel::Graph::Node->new ( '$a = 9;', 1);
is ($node->{_type}, 1, 'type got set');
is ($node->name(), "1", 'new name');
is ($node->label(), '$a = 9;', 'label');


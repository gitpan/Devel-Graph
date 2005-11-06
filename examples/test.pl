#!/usr/bin/perl -w

use lib 'lib';
use Devel::Graph;

my $format = shift || 'as_boxart';

my $g = Devel::Graph->new();

$g->add_block ('$a = "9";');
$g->add_block ('my $b = 1;');
$g->add_if_then ( 'if ($a == 9)', '$b == 9;' );
$g->add_if_then_else ( 'if ($b == 9)', '$b == $a + 1;', '$c == 1' );

$g->add_for ( 'my $i = 0;', 'for: $i < 10;', '$i++', '$a++;');

$g->finish();

my $gr = $g->as_graph();

print STDERR "Resulting graph has ", 
	scalar $gr->nodes(), " nodes and ", 
	scalar $gr->edges()," edges:\n\n";

binmode STDOUT, ':utf8' or die ("binmode STDERR, ':utf8' failed: $!");
print $gr->$format();


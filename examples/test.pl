#!/usr/bin/perl -w

use lib 'lib';
use Devel::Graph;

my $code = shift || 'examples/test.pl';
my $format = shift || 'as_ascii';

my $a = $code;
$a = \$code unless -f $code;		# code or file?

my $gr = Devel::Graph->graph($a);

#use PPI::Dumper; PPI::Dumper->new(PPI::Document->new($a), whitespace => 0)->print();

print STDERR "Resulting graph has ", 
	scalar $gr->nodes(), " nodes and ", 
	scalar $gr->edges()," edges:\n\n";

binmode STDOUT, ':utf8' or die ("binmode STDERR, ':utf8' failed: $!");
print $gr->$format();


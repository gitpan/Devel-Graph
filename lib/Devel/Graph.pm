#############################################################################
# Graph perl code as a Graph::Easy object
#
# (c) by Tels 2004-2005.
#############################################################################

package Devel::Graph;

use Graph::Easy;
use Graph::Easy::Base;
use Graph::Flowchart;

$VERSION = '0.04';
@ISA = qw/Graph::Easy::Base/;

use strict;
use warnings;

# XXX TODO
#use PPI;

#############################################################################
#############################################################################

sub _init
  {
  my ($self, $args) = @_;

  $self->{flow} = Graph::Flowchart->new();

  $self;
  }

sub graph
  {
  # decompose code and return as Graph::Easy object
  my ($self, $code) = shift;

  $self = $self->new() unless ref($self);

  $self->decompose($code);

  $self->{flow}->as_graph();			# return the Graph::Easy object
  }

sub as_graph
  {
  # return the internal Graph::Easy object
  my $self = shift;

  $self->{flow}->as_graph();
  }

sub as_flowchart
  {
  # return the internal Graph::Flowchart object
  my $self = shift;

  $self->{flow};
  }

sub decompose
  {
  my ($self, $code) = @_;

  $self->_init();				# clear data

  die ("decompose() not yet implemented. Sorry. Please bug Tels to fix this.");
  $self;
  }

1;
__END__

=head1 NAME

Devel::Graph - Turn Perl code into a Graph::Flowchart object

=head1 SYNOPSIS

	use Devel::Graph;

	my $graph = Devel::Graph->graph( '$a = 9 if $b == 1' );

	print $graph->as_ascii();

=head1 DESCRIPTION

This module decomposes Perl code into blocks and generates an Graph::Flowchart
object out of these. The resulting object represents the code in a
flowchart manner and it can return you a Graph::Easy object.

This in turn can be converted it into all output formats currently
supported by Graph::Easy, namely HTML, SVG, ASCII text etc.

B<Note:> The actual decomposing parts are not yet implemented. Currently
there is only code to assemble the flowchart manually via methods.

X<graph>
X<Perl>
X<code>
X<ascii>
X<html>
X<svg>
X<flowchart>
X<diagram>
X<decompose>

=head1 EXPORT

Exports nothing.

=head1 METHODS

C<graph()> provides a simple function-style interface, while all
other methods are for an object-oriented model.

=head2 graph()

	my $graph = Devel::Graph->graph( $code );

Takes Perl code in $code (as string or code ref) and returns a flowchart
as C<Graph::Easy> object.

This is a shortcut to avoid the OO interface described below and will
be equivalent to:

	my $grapher = Devel::Graph->new();
	$grapher->decompose( $code );
	my $graph = $grapher->as_graph();

Please see C<Graph::Easy> for further details on what to do with the
returned object.

=head2 new()

	my $grapher = Devel::Graph->new();

Creates a new C<Devel::Graph> object.

=head2 decompose()

	$grapher->decompose( $code );

Takes Perl code in $code (as string or code ref) and 
decomposes it into blocks and updates the internal
structures with a flowchart representing this code.

B<Note:> Not yet implemented.

=head2 as_graph()

	my $graph = $grapher->as_graph();

Return the internal data structure as C<Graph::Easy> object.

=head2 as_flowchart()

	my $chart = $grapher->as_flowchart();

Return the internal data structure as C<Graph::Flowchart> object.

=head1 SEE ALSO

L<Graph::Easy>, L<Graph::Flowchart>.

=head1 COPYRIGHT AND LICENSE

This library is free software; you can redistribute it and/or modify
it under the same terms of the GPL version 2.
See the LICENSE file for information.

X<gpl>

=head1 AUTHOR

Copyright (C) 2004-2005 by Tels L<http://bloodgate.com>

X<tels>
X<bloodgate.com>

=cut

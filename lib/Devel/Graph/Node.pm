#############################################################################
# A node in the graph (includes a type field)
#
# (c) by Tels 2004-2005.
#############################################################################

package Devel::Graph::Node;

@ISA = qw/Graph::Easy::Node Exporter/;

use warnings;
use Graph::Easy::Node;
use Exporter;

$VERSION = '0.01';

#############################################################################
#############################################################################

@EXPORT_OK = qw/
  N_START N_END N_BLOCK N_IF N_THEN N_ELSE N_JOINT N_END N_FOR N_BODY
  N_CONTINUE
  /;

use strict;

sub N_START ()		{ 1; }
sub N_END () 		{ 2; }
sub N_BLOCK ()		{ 3; }
sub N_IF ()		{ 4; }
sub N_THEN ()		{ 5; }
sub N_ELSE ()		{ 6; }
sub N_JOINT ()		{ 7; }
sub N_FOR ()		{ 8; }
sub N_BODY ()		{ 9; }
sub N_CONTINUE ()	{ 10; }

#############################################################################

sub new
  {
  my ($class, $label, $type) = @_;

  my $self = bless {}, $class;

  $type = N_START() unless defined $type;

  $self->{_type} = $type;
  $self->{id} = Graph::Easy::Node::_new_id();

  $self->_init( { label => $label, name => $self->{id} } );

  if ($type == N_JOINT)
    {
    $self->set_attribute('shape', 'point');
    }
  elsif ($type == N_IF)
    {
    $self->set_attribute('shape', 'diamond');
    }
  elsif ($type == N_END || $type == N_START)
    {
    $self->set_attribute('border-style', 'bold');
    }

  $self;
  }

1;
__END__

=head1 NAME

Devel::Graph::Node - A node in a Devel::Graph, representing a block/expression

=head1 SYNOPSIS

	use Devel::Graph;

	my $graph = Devel::Graph->graph( '$a = 9 if $b == 1' );

	print $graph->as_ascii();

=head1 DESCRIPTION

This module is used by Devel::Graph internally, there should be no need to
use it directly.

=head2 EXPORT

Exports nothing on default but can export on request the following:

  N_START N_END N_BLOCK N_IF N_THEN N_ELSE N_JOINT N_END N_FOR N_BODY
  N_CONTINUE

=head1 SEE ALSO

L<Devel::Graph>.

=head1 COPYRIGHT AND LICENSE

This library is free software; you can redistribute it and/or modify
it under the same terms of the GPL version 2.
See the LICENSE file for information.

X<gpl>

=head1 AUTHOR

Copyright (C) 2004-2005 by Tels L<http://bloodgate.com>

X<tels>

=cut

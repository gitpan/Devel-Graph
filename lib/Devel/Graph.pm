#############################################################################
# Graph perl code as a Graph::Easy object
#
# (c) by Tels 2004-2005.
#############################################################################

package Devel::Graph;

$VERSION = '0.02';

use strict;
use warnings;

use Graph::Easy;
use Devel::Graph::Node qw/
  N_IF N_THEN N_ELSE
  N_END N_START N_BLOCK N_JOINT
  N_FOR N_CONTINUE
  /;

#############################################################################
#############################################################################

sub new
  {
  my $class = shift;

  my $self = bless {}, $class;

  my $args = $_[0];
  $args = { @_ } if ref($args) ne 'HASH';

  $self->_init($args);
  }

sub _init
  {
  my ($self, $args) = @_;

  $self->{graph} = Graph::Easy->new();

  # make the chart flow down
  my $g = $self->{graph};
  $g->set_attribute('flow', 'down');

  # add the start node
  $self->{_last} = $self->new_block ('start', N_START() );

  $g->add_node($self->{_last});
#  $g->debug(1);

  $self->{_first} = $self->{_last};
  $self->{_cur} = $self->{_last};

  $self;
  }

sub graph
  {
  # decompose code and return as Graph::Easy object
  my ($self, $code) = shift;

  $self = $self->new() unless ref($self);

  $self->decompose($code);

  $self->{graph};				# return the Graph::Easy object
  }

sub as_graph
  {
  # return the internal Graph::Easy object
  my $self = shift;

  $self->{graph};
  }

sub decompose
  {
  my ($self, $code) = @_;

  $self->_init();				# clear data
	
  $self;
  }

#############################################################################

sub last_block
  {
  # return the last block
  my $self = shift;

  $self->{_last};
  }

sub current_block
  {
  # return the current insertion point
  my $self = shift;

  $self->{_cur};
  }

sub first_block
  {
  # return the first block
  my $self = shift;

  $self->{_first};
  }

#############################################################################

sub new_block
  {
  my ($self, $label, $type) = @_;

  Devel::Graph::Node->new( $label, $type );    
  }

#############################################################################

sub merge_blocks
  {
  # if possible, merge the given two blocks
  my ($self, $first, $second) = @_;

  # see if we should merge the blocks

  return $second
	if ( ($first->{_type} != N_JOINT()) &&
	     ($first->{_type} != $second->{_type} ) );
   
  my $g = $self->{graph};

  my $label = $first->label();
  $label .= '\n' unless $label eq '';
  $label .= $second->label();

#  print STDERR "# merge $first->{name} $second->{name}\n";

  if ($first->{_type} == N_JOINT)
    {
    $first->del_attribute('shape');

    for my $att (qw/shape border-style/)
      {
      my $a = $second->attribute($att);
      $first->set_attribute($att, $a) if defined $a;
      }
    }
  $first->set_attribute('label', $label);
  $first->{_type} = $second->{_type};

  # drop second node from graph
  $g->merge_nodes($first, $second);

  $self->{_cur} = $first;
  }

#############################################################################

sub connect
  {
  my ($self, $from, $to, $edge_label) = @_;

  my $g = $self->{graph};
  my $edge = $g->add_edge($from, $to);

  $edge->set_attribute('label', $edge_label) if defined $edge_label;

  $edge;
  }

sub add_block
  {
  my ($self, $block, $where) = @_;

  $block = $self->new_block($block, N_BLOCK() ) unless ref $block;

  $where = $self->{_cur} unless defined $where;
  my $g = $self->{graph};

  $g->add_edge($where, $block);

  $block = $self->merge_blocks($where, $block);

  $self->{_cur} = $block;			# set new _cur and return it
  }

sub add_joint
  {
  my $self = shift;

  my $g = $self->{graph};

  my $joint = $self->new_block('', N_JOINT());
  $g->add_node($joint);

  # connect the requested connection points to the joint
  for my $node ( @_ )
    {
    $g->add_edge($node, $joint);
    }

  $joint;
  }

sub add_if_then
  {
  my ($self, $if, $then, $where) = @_;
 
  $if = $self->new_block($if, N_IF()) unless ref $if;
  $then = $self->new_block($then, N_THEN()) unless ref $then;

  $where = $self->{_cur} unless defined $where;
  my $g = $self->{graph};

  $if = $self->add_block ($if, $where);

  $self->connect($if, $then, 'true');

  # then --> '*'
  $self->{_cur} = $self->add_joint($then);

  # if -- false --> '*'
  $self->connect($if, $self->{_cur}, 'false');

  $self->{_cur};
  }

sub add_if_then_else
  {
  my ($self, $if, $then, $else, $where) = @_;
 
  $if = $self->new_block($if, N_IF()) unless ref $if;
  $then = $self->new_block($then, N_THEN()) unless ref $then;
  $else = $self->new_block($else, N_ELSE()) unless ref $else;

  $where = $self->{_cur} unless defined $where;
  my $g = $self->{graph};

  $if = $self->add_block ($if, $where);
  
#  $if->set_attribute('rows',2);

  $self->connect($if, $then, 'true');
  $self->connect($if, $else, 'false');

  # then --> '*', else --> '*'
  $self->{_cur} = $self->add_joint($then, $else);

  $self->{_cur};
  }

sub add_for
  {
  # add a for (my $i = 0; $i < 12; $i++) style loop
  my ($self, $init, $while, $cont, $body, $where) = @_;
 
  $init = $self->new_block($init, N_FOR()) unless ref $init;
  $while = $self->new_block($while, N_IF()) unless ref $while;
  $cont = $self->new_block($cont, N_CONTINUE()) unless ref $cont;
  $body = $self->new_block($body, N_BLOCK()) unless ref $body;

  # init -> if $while --> body --> cont --> (back to if)

  $where = $self->{_cur} unless defined $where;
  my $g = $self->{graph};

  $init = $self->add_block ($init, $where);
  $while = $self->add_block ($while, $init);

  $while->set_attribute('rows',2);

  $self->connect($while, $body, 'true');

  $self->connect($body, $cont);
  $self->connect($cont, $while);

  my $joint = $self->add_joint();
  $self->connect($while, $joint, 'false');

  $self->{_cur} = $joint;

  ($joint, $body);
  }

#############################################################################

sub finish
  {
  my ($self, $where) = @_;

  my $g = $self->{graph};

  my $end = $self->new_block ( 'end', N_END() );

  $end = $self->add_block ($end, $where);
 
  $self->{_last} = $end;

  $self;
  }

1;
__END__

=head1 NAME

Devel::Graph - Turn Perl code into an Graph::Easy object

=head1 SYNOPSIS

	use Devel::Graph;

	my $graph = Devel::Graph->graph( '$a = 9 if $b == 1' );

	print $graph->as_ascii();

=head1 DESCRIPTION

This module decomposes Perl code into blocks and generates an Graph::Easy
object out of these. The resulting object represents the code in a
flowchart manner and you can turn it into all output formats currently
supported by Graph::Easy, namely HTML, SVG, ASCII text etc.

X<graph>
X<Perl>
X<code>
X<ascii>
X<html>
X<svg>
X<flowchart>
X<diagram>

=head1 EXPORT

Exports nothing.

=head1 METHODS

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

=head2 as_graph()

	my $graph = $grapher->as_graph();

Return the internal data structure as C<Graph::Easy> object.

=head2 current_block()

	my $insertion = $grapher->current_block();

Returns the current block in the flow chart, e.g. where new code blocks
will be inserted by the C<add_*> methods.

=head2 first_block()

	my $first = $grapher->first_block();

Returns the first block in the flow chart, usually the 'start' block.

=head2 last_block()

	my $last = $grapher->first_block();

Returns the last block in the flow chart, usually the block where you
last added something via one of the C<add_*> routines.

=head2 finish()

	$grapher->finish( $block );
	$grapher->finish( );

Adds an end-block. If no parameter is given, uses the current position,
otherwise appends the end block to the given C<$block>. See C<current_block>.

=head2 new_block()

	my $block = $grapher->add_block( $code );
	my $block = $grapher->add_block( $code, Devel::Graph::Node::N_BLOCK );

Creates a new block/node from the given code and the optional type.

=head2 add_block()

	my $current = $grapher->add_block( $block );
	my $current = $grapher->add_block( $block, $where );

Add the given block. See C<new_block> on creating the block before hand.

The optional C<$where> parameter is the point where the code will be
inserted. If not specified, it will be appended to the current block,
see C<current_block>.

Returns the new current block.

=head2 add_if_then()

	my $grapher->add_if_then( $if, $then);
	my $grapher->add_if_then( $if, $then, $where);

Add an if-then branch to the flowchart. The optional C<$where> parameter
defines at which block to attach the construct.

=head2 add_if_then_else()

	my $grapher->add_if_then_else( $if, $then, $else);
	my $grapher->add_if_then_else( $if, $then, $else, $where);

Add an if-then-else branch to the flowchart.

The optional C<$where> parameter defines at which block to attach the
construct.

=head2 add_for()

	my $grapher->add_for( $init, $while, $cont, $body);
	my $grapher->add_for( $init, $while, $cont, $body, $where);

Add a C<< for (my $i = 0; $i < 12; $i++) { ... } >> style loop.

The optional C<$where> parameter defines at which block to attach the
construct.

=head2 add_joint()

	my $joint = $grapher->add_joint( @blocks );

Adds a joint (an unlabeled, star-shaped node) to the flowchart and then
connects each block in the given list to that joint. This is used
f.i. by if-then-else constructs that need a common joint where all
the branches join together again.

=head2 merge_blocks()

	$grapher->merge_blocks($first,$second);

If possible, merge the given two blocks into one block, keeping all connections
to the first, and all from the second. Any connections between the two
blocks is dropped.

=head2 connect()

	my $edge = $grapher->connect( $from, $to );
	my $edge = $grapher->connect( $from, $to, $edge_label );

Connects two blocks with an edge, setting the optional edge label.

Returns the <Graph::Easy::Edge> object.
 
=head1 SEE ALSO

L<Graph::Easy>.

=head1 COPYRIGHT AND LICENSE

This library is free software; you can redistribute it and/or modify
it under the same terms of the GPL version 2.
See the LICENSE file for information.

X<gpl>

=head1 AUTHOR

Copyright (C) 2004-2005 by Tels L<http://bloodgate.com>

X<tels>

=cut

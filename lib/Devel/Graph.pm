#############################################################################
# Generate flowchart from perl code
#
# (c) by Tels 2004-2006.
#############################################################################

package Devel::Graph;

use Graph::Easy;
use Graph::Easy::Base;
use Graph::Flowchart;
use Graph::Flowchart::Node qw/
  N_IF N_THEN N_ELSE N_JOINT N_BLOCK
  N_SUB N_RETURN N_CONTINUE
  /;
require Exporter;

$VERSION = '0.06';
@ISA = qw/Graph::Easy::Base Exporter/;
@EXPORT_OK = qw/graph/;

use strict;
use PPI;

#############################################################################
#############################################################################

sub _init
  {
  my ($self, $args) = @_;

  $self->reset();

  $self;
  }

sub graph
  {
  # decompose code and return as Graph::Easy object

  # allow Devel::Graph->graph() calling style
  my $class = 'Devel::Graph';
  $class = shift if @_ == 2; $class = ref($class) if ref($class);
  my $code = shift;

  my $self = $class->new();
  $self->reset();
  $self->decompose($code);
  $self->{flow}->finish();

  $self->{flow}->as_graph();			# return the Graph::Easy object
  }

sub as_graph
  {
  # return the internal Graph::Easy object
  my $self = shift;

  $self->{flow}->as_graph();
  }

sub as_ascii
  {
  # return the flowchart as ASCII
  my $self = shift;

  $self->{flow}->as_graph()->as_ascii();
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

  $self->_croak("Expected SCALAR ref, but got " . ref($code))
   if ref($code) && ref($code) ne 'SCALAR';

  $self->_croak("Got filename '$code', but can't read it: $!")
   if !ref($code) && !-f $code;

  my $doc = PPI::Document->new($code);

  $self->_croak ("Couldn't create PPI::Document from $code")
   unless ref($doc);

  $self->_parse($doc);

  $self;
  }

sub finish
  {
  my $self = shift;

  $self->{flow}->finish();
  }

sub reset
  {
  # reset the internal structure
  my $self = shift;

  $self->{cur_group} = undef;
  $self->{stack} = [];
  $self->{flow} = Graph::Flowchart->new();

  $self;
  }

#############################################################################
#############################################################################
# _parse helper routines

sub _find_first
  {
  # return the first child of $element matching any of the given types
  my $self = shift;
  my $element = shift;

  for my $child (@{$element->{children}})
    {
    for my $type (@_)
      {
      return $child if $child->isa($type);
      }
    }

  $self->_croak ("Couldn't find any children of " .
    ref($element) . " of types: " . join(", ",@_)." ");   
  undef;
  }

sub _find_second
  {
  # return the first child of $element matching any of the given types
  my $self = shift;
  my $element = shift;

  my @blocks;
  for my $child (@{$element->{children}})
    {
    for my $type (@_)
      {
      push @blocks, $child if $child->isa($type);
      return $blocks[-1] if scalar @blocks == 2;
      }
    }

  $self->_croak ("Couldn't find second children of " .
    ref($element) . " of types: " . join(", ",@_)." ");   
  }

sub _parse_compound
  {
  my ($self, $element, $type) = @_;

  # work around bug in PPI
  $type = $element->type() unless defined $type;

  $self->_croak("Cannot determine type of compound element $element")
    unless defined $type;
 
  return $self->_parse_loop($element)
    if $type eq 'for' || $type eq 'foreach';

  # ignoring whitespace and comments, find the condition
  my @blocks;

  for my $child (@{$element->{children}})
    {
    push @blocks, $child if
      $child->isa('PPI::Structure::Condition') ||      
      $child->isa('PPI::Structure::Block');
    }
   
  return $self->_parse_if(@blocks)
    if $type eq 'if';

  return $self->_parse_while($type, @blocks)
    if $type =~ /^(until|while)\z/;
  }

sub _parse_if
  {
  my ($self, $condition, $block) = @_;

  my $flow = $self->{flow};

  # cur => if => then => joint
  #        |--------------^
  
  my $if = $flow->new_block( 'if ' . $condition->content(), N_IF());
  $if = $flow->add_block($if);

  # cur => if => then
  my $then = $flow->add_joint(); $flow->connect($if,$then,'true');
  $flow->current_block($then);

  # fill in the "then" block
  $self->_parse($block);

  # cur => if => then => joint
  #        ----false--------^

  my $else = $flow->add_new_joint();
  $flow->connect($if,$else,'false');
  $flow->current($else);
  }

sub _parse_unless
  {
  my ($self, $condition, $block) = @_;

  my $flow = $self->{flow};

  # cur => if => then => joint
  #        |--------------^
  
  my $if = $flow->new_block( 'if (! ' . $condition->content() . ' )', N_IF());
  $if = $flow->add_block($if);

  # cur => if => then
  my $then = $flow->add_joint(); $flow->connect($if,$then,'true');
  $flow->current_block($then);

  # fill in the "then" block
  $self->_parse($block);

  # cur => if => then => joint
  #        ----false--------^

  my $else = $flow->add_new_joint();
  $flow->connect($if,$else,'false');
  $flow->current($else);
  }

sub _find_on_stack
  {
  my ($self, $type) = @_;

  my $stack = $self->{stack};

  for my $e (@$stack)
    {
    return $e if $e->{_type} == $type;
    }

  $self->_croak("Couldn't find type $type on stack");
  }

sub _parse_sub
  {
  my ($self, $sub) = @_;

  my $name = $self->_find_second($sub, 'PPI::Token::Word');
  
  my $flow = $self->{flow};

  $flow->add_group("sub $name:");

  # remember pos before sub
  my $cur = $flow->current();

  # entry point
  my $joint = $flow->add_joint(); $joint->{_label} = $name;
  $flow->current($joint);

  # recurse into our children, but ignore:
  # whitespace and comments and Null (";")
  foreach my $child (@{$sub->{children}})
    {
    $self->_parse($child)
      unless $child->isa('PPI::Token::Whitespace') ||
             $child->isa('PPI::Token::Comment') ||
             $child->isa('PPI::Statement::Null');
    }

  # continue at pos before sub
  $flow->current($cur);
  $flow->no_group();

  }

sub _parse_while
  {
  my ($self, $type, $condition, $body, $continue) = @_;

  my $c = undef; $c = '' if defined $continue;
  my $flow = $self->{flow};
  my $method = "add_$type";
  my ($cur_block,$body_block, $cont_block) = $flow->$method( "$type ".$condition->content(), '', $c );

  # descent into the body block
  $flow->current($body_block);
  $self->_parse($body);
  if (defined $continue)
    {
    $flow->current($cont_block);
    $cont_block->_set_type(N_JOINT());		# so that the parsed blocks get combined
    $self->_parse($continue);
    $cont_block->_set_type(N_CONTINUE());	# set right type
    }
  $flow->current($cur_block);
  }

sub _parse_loop
  {
  my ($self, $element) = @_;

#  PPI::Statement::Compound
#    PPI::Token::Word    'for'
#    PPI::Structure::ForLoop     ( ... )
#      PPI::Statement::Variable
#        PPI::Token::Word        'my'
#        PPI::Token::Symbol      '$i'
#        PPI::Token::Operator    '='
#        PPI::Token::Number      '0'
#        PPI::Token::Structure   ';'
#      PPI::Statement
#        PPI::Token::Symbol      '$i'
#        PPI::Token::Operator    '<'
#        PPI::Token::Number      '10'
#        PPI::Token::Structure   ';'
#      PPI::Statement
#        PPI::Token::Symbol      '$i'
#        PPI::Token::Operator    '++'
#    PPI::Structure::Block       { ... }
#      PPI::Statement
#        PPI::Token::Symbol      '$b'
#        PPI::Token::Operator    '++'
#        PPI::Token::Structure   ';'

#  PPI::Statement::Compound
#    PPI::Token::Word    'for'
#    PPI::Token::Word    'my'
#    PPI::Token::Symbol          '$i'
#    PPI::Structure::ForLoop     ( ... )
#      PPI::Statement
#        PPI::Token::Symbol      '@list'
#    PPI::Structure::Block       { ... }
#      PPI::Statement
#        PPI::Token::Word        'print'
#        PPI::Token::Symbol      '$foo'

  my $loop = $self->_find_first($element, 'PPI::Structure::ForLoop');

  my (@bodies, @blocks, @var);
  # get the stuff inside the ()
  foreach my $child (@{$loop->{children}})
    {
    push @blocks, $child->content() if $child->isa('PPI::Statement');
    }
  # get the body (and continue) block
  foreach my $child (@{$element->{children}})
    {
    push @bodies, $child if $child->isa('PPI::Structure::Block');
    }
  # get the variable in front of the () for foreach loops
  foreach my $child (@{$element->{children}})
    {
    push @var, $child->content() if $child->isa('PPI::Token::Word') || $child->isa('PPI::Token::Symbol');
    }
  shift @var;	# remove the "for" so that "for my $i" results in "my $i";

  my ($cur_block,$body_block, $cont_block);
  my $flow = $self->{flow};
  if (@blocks == 1)
    {
    # 'for my $var (@list)'
    my $v = join(" ", @var);
    $blocks[0] = 'for ' . $v . " ($blocks[0])";
    push @blocks, '';

    ($cur_block,$body_block, $cont_block) = $flow->add_foreach( @blocks );
    }
  else
    {
    push @blocks, '';
    push @blocks, '' if @bodies > 1;	# have a "continue {} " block?
  
    $blocks[0] = 'for: ' . $blocks[0];
    $blocks[1] = 'if ' . $blocks[1];

    ($cur_block,$body_block, $cont_block) = $flow->add_for( @blocks );
    }

  $flow->current($body_block);
  $self->_parse($bodies[0]);
  if (@bodies > 1)
    {
    $flow->current($cont_block);
    $self->_parse($bodies[1]);
    }
  $flow->current($cur_block);

  }

sub _parse_conditional
  {
  my ($self, $element) = @_;
  
  # gather all elements up to the condition
  my @blocks;
  my $condition;

  for my $child (@{$element->{children}})
    {
    next unless $child->significant();		# ignore whitespace etc
    $condition = $child, last if $child->isa('PPI::Structure::Condition');
    push @blocks, $child;
    }

  my $type = pop @blocks;

  my $block = $element->clone();
  # delete the condition
  my $c = $block->find_first('PPI::Structure::Condition');
  $c->delete();

  # and delete the word ('if', 'unless' etc) immidiately before it
  $c = $block->find_first('PPI::Token::Word');
  $c->delete();

  # delete trailing whitespace in $block (so that "$c = 123 ;" turns in "$c = 123;"
  for my $child (reverse @{$block->{children}})
    {
    # stop at the first significant child other than the ";"
    next if $child->isa('PPI::Token::Structure');
    last if $child->significant(); 
    $child->delete();
    }
#  print " after delete: $block\n";

  if ($type eq 'if')
    {
    return $self->_parse_if($condition, $block);
    }
  elsif ($type eq 'unless')
    {
    return $self->_parse_unless($condition, $block);
    }
  }

sub _parse_break
  {
  my ($self, $element) = @_;

  # find the type of the break statement
  my $type = $self->_find_first($element, 'PPI::Token::Word');

  my $flow = $self->{flow};

  my $target;
  if ($type ne 'return')
    {
    # ignore first Token::Word
    $target = $self->_find_second('PPI::Token::Word');
    $flow->add_jump($element->content(), $type->content(), '', $target);
    }
  else
    {
    $flow->add_new_block($element->content(), N_RETURN(), '');
    }
  }

#############################################################################

sub _parse_expression
  {
  my ($self, $element) = @_;

  my $flow = $self->{flow};

  $flow->add_block($element->content());
  }

#############################################################################
#############################################################################
# main parse routine, recursive

sub _parse
  {
  # take a PPI::ELement and descend into it recursively
  no warnings 'recursion';
  my ($self, $element) = @_;

#  print STDERR "parsing ", ref($element),"\n";

  # handle if, while, for
  return $self->_parse_compound($element)
    if $element->isa('PPI::Statement::Compound');

  ########################################################################
  ########################################################################
  # work around bug in PPI not parsing "until" as Compound
  my $w;
  $w = $element->first_token() if $element->isa('PPI::Statement'); 
 
  return $self->_parse_compound($element, 'until')
    if defined $w && $w eq 'until';
  ########################################################################
  ########################################################################

  # handle sub 
  return $self->_parse_sub($element)
    if $element->isa('PPI::Statement::Sub');

  # handle next, last, return and break
  return $self->_parse_break($element)
    if $element->isa('PPI::Statement::Break');

  return $self->_parse_conditional($element)
    if (ref($element) eq 'PPI::Statement' && $element->find_any('PPI::Structure::Condition'));

  # handle normal expressions like:
  # "$a == 1"
  # "use strict;"
  return $self->_parse_expression($element)
    if (($element->isa('PPI::Statement')));#   ||
#        ($element->isa('PPI::Statement::Expression')) ||
#        ($element->isa('PPI::Statement::Include'))    );

  # recurse into our children, but ignore:
  # whitespace, comments, Null (";") etc
  if ($element->isa('PPI::Node'))
    {
    foreach my $child (@{$element->{children}})
      {
      $self->_parse($child) if $child->significant();
      }
    }
  }

1;
__END__

=head1 NAME

Devel::Graph - Turn Perl code into a Graph::Flowchart object

=head1 SYNOPSIS

	use Devel::Graph;

	my $graph = Devel::Graph->graph( \'$a = 9 if $b == 1' );

	print $graph->as_ascii();

=head1 DESCRIPTION

This module decomposes Perl code into blocks and generates a
L<Graph::Flowchart> object out of these. The resulting object represents the
code in a flowchart manner and it can return you a L<Graph::Easy> object.

This in turn can be converted it into all output formats currently
supported by C<Graph::Easy>, namely HTML, SVG, ASCII art, Unicode art,
graphviz text etc.

=head2 Parsing

The parsing is done by L<PPI>, so everything that is supported
properly by PPI should work.

X<graph>
X<Perl>
X<code>
X<structure>
X<analysis>
X<ascii>
X<html>
X<svg>
X<flowchart>
X<diagram>
X<decompose>

=head2 Customizing the flowchart

Per default, the flowchart will have certain properties, like bold start/end
blocks, diamond-shaped C<if>-blocks and so on. You can change these
by setting class attributes on the returned graph object:

	use Devel::Graph;
	my $g = Devel::Graph->graph( '$a = 9 if $b == 1' );

	$g->set_attribute('node.if', 'fill', 'red');    # if blocks: red
	$g->set_attribute('node.for', 'fill', 'blue');  # for blocks: blue
	$g->set_attribute('edge.true', 'style', 'bold');# true edges: bold
	print $g->as_html_file();

Subclasses for C<node> include C<if>, C<for>, C<start>, C<end>, C<continue> etc.
For a list of all possible classes see C<Graph::Flowchart>, and for a list
of all possible attributes and their values, see C<Graph::Easy>.

=head1 EXPORT

Exports nothing.

=head1 METHODS

C<graph()> provides a simple function-style interface, while all
other methods are for an object-oriented model.

=head2 graph()

	my $graph = Devel::Graph->graph( \$code );
	my $graph = Devel::Graph->graph( $filename );

Takes Perl code in $code (as SCALAR ref or scalar filename) and returns a flowchart
as C<Graph::Easy> object.

This is a shortcut to avoid the OO interface described below and will
be equivalent to:

	my $code = \'$a = 9;';
	my $flow = Devel::Graph->new();
	$flow->decompose( $code );
	$flow->finish( $code );
	my $graph = $grapher->as_graph();

Please see C<Graph::Easy> for further details on what to do with the
returned object.

=head2 new()

	my $flow = Devel::Graph->new();

Creates a new C<Devel::Graph> object.

=head2 decompose()

	$flow->decompose( \$code );		# \'$a = 1;'
	$flow->decompose( $filename );		# 'lib/Package.pm'

Takes Perl code (scalar ref in C<$code>) or Perl file (filename in C<$code>) and 
decomposes it into blocks and updates the internal structures with a flowchart
representing this code.

If called more than one time, the code will be added to the flowchart. To
get a new, empty flowchart, use C<reset()>.

=head2 finish()

	$flow->finish();

Finish the flowchart by attaching an end node to the current node.

=head2 reset()

	$flow->reset();

Reset the internal state of the object, so that decompose() will create
a new flowchart.

=head2 as_graph()

	my $graph = $flow->as_graph();

Return the internal data structure as C<Graph::Easy> object.

=head2 as_ascii()

	print $flow->as_ascii();

Return the flow chart as ASCII art. Shortcut for
C<$grapher->as_graph->as_ascii()>.

=head2 as_flowchart()

	my $chart = $flow->as_flowchart();

Return the internal data structure as C<Graph::Flowchart> object.

=head1 BUGS

Not all Perl constructs are implemented yet, especially the more esoteric
Perl constructs.

Also, things like C<<$a = 9 if $b == 9>> (no C< () > around
the condition) are buggy and/or incomplete.

=head1 SEE ALSO

L<Graph::Easy>, L<Graph::Flowchart>, L<PPI>.

=head1 COPYRIGHT AND LICENSE

This library is free software; you can redistribute it and/or modify
it under the same terms of the GPL version 2.
See the LICENSE file for information.

X<gpl>

=head1 AUTHOR

Copyright (C) 2004-2006 by Tels L<http://bloodgate.com>

X<tels>
X<bloodgate.com>

=cut

package NoSQL::PL2SQL::DBI::Null ;

sub AUTOLOAD {
	my $self = shift ;
	my $sql = shift ;
	return $sql ;
	}

sub DESTROY {}

package NoSQL::PL2SQL::DBI ;

use 5.008009;
use strict;
use warnings;
use DBI ;
use XML::Parser::Nodes ;

require Exporter;

our @ISA = qw(Exporter);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration	use NoSQL::PL2SQL::Node ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
our %EXPORT_TAGS = ( 'all' => [ qw() ] ) ;

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } ) ;

our @EXPORT = qw() ;

our $VERSION = '0.04';

# Preloaded methods go here.

my @sqllog ;
my $nulldbi = bless \( my $null = 'NoSQL::PL2SQL::DBI::Null' ), 
			'NoSQL::PL2SQL::DBI::Null' ;

## I started looking for an XML based schema that is implementation
## independent.  This one, my own invention and based on MySQL, was
## originally a placeholder.  Suffice that it remains a TODO...

my $xmlschema =<<'endschema' ;
<mysql>
  <sql>
    <table command="CREATE" table="%s">
      <column name="id" type="INT">
	  <PRIMARY />
	  <KEY />
	  <AUTO_INCREMENT />
      </column>
      <column name="objectid" type="INT" />
      <column name="objecttype" type="VARCHAR" length="40" />
      <column name="stringrepr" type="VARCHAR" length="40" />
      <column name="intdata" type="INT" />
      <column name="doubledata" type="DOUBLE" />
      <column name="stringdata" type="VARCHAR" length="512" />
      <column name="textkey" type="VARCHAR" length="256" />
      <column name="intkey" type="INT" />
      <column name="blesstype" type="VARCHAR" length="40" />
      <column name="reftype" type="VARCHAR" length="10">
	  <NOT /><NULL />
      </column>
      <column name="item" type="INT">
	  <UNSIGNED />
      </column>
      <column name="refto" type="INT">
	  <UNSIGNED />
      </column>
      <column name="chainedstring" type="INT">
	  <UNSIGNED />
      </column>
      <column name="defined" type="TINYINT" />
      <column name="deleted" type="TINYINT" />
    </table>
  </sql>
  <sql>
    <index name="reference" command="CREATE" table="%s">
      <column name="objectid" />
      <column name="objecttype" />
      <column name="reftype" />
    </index>
  </sql>
</mysql>
endschema

sub schema {
	my $self = shift ;
	my $package = ref $self || $self ;
	my $schema = @_? shift( @_ ): $xmlschema ;

	my $nodes = bless XML::Parser::Nodes->new( $schema ), 
			join( '::', $package, 'Schema' ) ;

	return $nodes->schema ;
	}

sub sqldump {
	shift @_ ;
	@sqllog = () if @_ ;
	return @sqllog ;
	}

sub debug {
	push @sqllog, $_[-1] if @_ ;
	}

sub sqlstatement {
	my $self = shift ;
	my $sprintf = shift ;
	return sprintf $sprintf, $self->[1] ;
	}

sub do  {
	my $self = shift ;
	push @sqllog, my $sql = $self->sqlstatement( @_ ) ;
	return $self->db->do( $sql ) ;
	}

sub rows_hash {
	my $self = shift ;
	push @sqllog, my $sql = $self->sqlstatement( @_ ) ;
	my $st = $self->db->prepare( $sql ) ;
	$st->execute ;
	my @out = () ;
	my $o ;
	push @out, { %$o } while $o = $st->fetchrow_hashref ;
	return $out[0] unless wantarray ;
	return @out ;
	}

sub rows_array {
	my $self = shift ;
	push @sqllog, my $sql = $self->sqlstatement( @_ ) ;
	my $st = $self->db->prepare( $sql ) ;
	$st->execute ;
	my @out = () ;
	my $o ;
	push @out, [ @$o ] while $o = $st->fetchrow_arrayref ;
	return $out[0] unless wantarray ;
	return @out ;
	}

sub perldata {
	my $self = shift ;
	my %r = map { $_->{id} => $_ } $self->rows_hash( @_ ) ;
	return \%r ;
#	return bless \%r, 'NoSQL::PL2SQL::Perldata' ; ## see perlmonks #989671
	}

sub new {
	my $package = shift ;
	my $tablename = shift ;
	return bless [ $nulldbi, $tablename ], $package ;
	}

sub connect {
	my $self = shift ;
	$self->[0] = DBI->connect( @_ ) ;
	return $self ;
	}

sub table {
	my $self = shift ;
	return $self->[1] ;
	}

sub db {
	my $self = shift ;
	return $self->[0] ;
	}

sub dbconnected {
	my $self = shift ;
	my $db = $self->db ;
	my $unconnected = $db->isa('SCALAR') && $$db eq ref $db ;
	return ! $unconnected ;
	}

## Implementation Specific
sub delete {
	my $self = shift ;
	my $id = shift ;

	return $self->do( sprintf 'DELETE FROM %s WHERE id=%d', '%s', $id ) ;
	}

## Implementation Specific
sub lastinsertid {
	my $self = shift ;
	my $db = $self->db ;
	return ! $self->dbconnected? 0:
			$db->last_insert_id( 
			  undef, undef, $self->table, 'id' ) ;
	}

## update() method is used for SQL "INSERT" and "UPDATE" constructions.
## Implementation Specific.  Default method is MySQL syntax.
sub update {
	my $self = shift ;
	my $id = shift ;

##	Each subsequent argument is an NVP array reference.  An optional
##	third element, if true, indicates a string value

	my @pairsf = ( '%s=%s', '%s="%s"', '%s=NULL' ) ;
	my @termsf = ( '%s', '"%s"', 'NULL' ) ;

	my $keys = join ',', map { $_->[0] } @_ ;
	my $values = join ',', map {
			sprintf $termsf[ defined $_->[1]? 
			  $_->[2] || length $_->[1] == 0: 2 ], 
			$self->stringencode( $_->[1], ! $_->[2] ) ; 
			} @_ ;
	my $nvp = join ',', map {
			sprintf $pairsf[ defined $_->[1]? 
			  $_->[2] || length $_->[1] == 0: 2 ], 
			$_->[0], $self->stringencode( $_->[1], ! $_->[2] )
			} @_ ;

	## User data should never be passed to sqlstatement()
	my $update = $self->sqlstatement( 'UPDATE %s' ) ;
	my $insert = $self->sqlstatement( 'INSERT INTO %s' ) ;
	push @sqllog, my $sql = defined $id? 
			"$update SET $nvp WHERE id=$id":
			"$insert ($keys) VALUES ($values)" ;
	my $sqlresults = $self->db->do( $sql ) ;	## do not combine
	return { id => $id || $self->lastinsertid,
			sqlresults => $sqlresults } ;
	}

sub insert {
	my $self = shift ;
	return $self->update( undef, @_ ) ;
	}

## Implementation Specific.  Default method is MySQL syntax.
sub fetch {
	my $self = shift ;

	my @pairsf = ( '%s=%s', '%s="%s"', '%s=NULL' ) ;
	my $nvp = join ' AND ', map {
			sprintf $pairsf[ defined $_->[1]? 
			  $_->[2] || length $_->[1] == 0: 2 ],
			$_->[0], $self->stringencode( $_->[1], ! $_->[2] )
			} @_ ;

	my $sql = "SELECT * FROM %s WHERE $nvp" ;
	return $self->dbconnected?
			$self->perldata( $sql ):
			$self->do( $sql ) ;
	}

## Implementation Specific.  Default method is MySQL syntax.
sub stringencode {
	my $self = shift ;
	my $text = shift ;
	return $text unless defined $text ;
	return $text if @_ && $_[0] ;
	$text =~ s/"/""/gs ;
	$text =~ s/\\/\\\\/gs ;
	return $text ;
	}

sub AUTOLOAD {
	my $self = shift ;
	my $sql = shift ;
	my $package = ref $self ;

	use vars qw( $AUTOLOAD ) ;
	my $func = $AUTOLOAD ;
	$func =~ s/^${package}::// ;
	return if $func eq 'DESTROY' ;

	my $cmd = sprintf '$self->db->%s( sprintf $sql || "", $self->table )', 
			$func ;
	return eval $cmd ;
	}

sub loadschema {
	my $self = shift ;
	return map { $self->do( $_ ) } $self->schema( @_ ) ;
	}


package NoSQL::PL2SQL::DBI::Schema ;
use base qw( XML::Parser::Nodes ) ;

sub schema {
	shift @_ unless ref $_[0] ;
	my $self = shift ;

	my @mysql = $self->childnode('mysql') ;
	return map { $self->new( $_ )->schema } 
			@mysql? $mysql[0]->childnodes: $self->childnodes ;
	}

sub new {
	my $self = shift ;
	my $nodechild = shift ;
	my $package = ref $self ;
	my @package = () ;

	my @nodenames = () ;
	my @refself = split /::/, $package ;
	push @nodenames, pop @refself 
			while @refself && $refself[-1] ne 'Schema' ;

	push @package, join '::', $package, $nodechild->[0] ;
	return bless $nodechild->[1], $package[-1]
			if eval join '', '@', $package[-1], '::ISA' ;

	push @package, join '::', __PACKAGE__, @nodenames, $nodechild->[0] ;
	return bless $nodechild->[1], $package[-1]
			if eval join '', '@', $package[-1], '::ISA' ;

	return bless $nodechild->[1], join '::', @refself ;
	}

sub command {
	my $self = shift ;
	my $command = $self->getattributes->{command} || '' ;

	return eval sprintf '$self->%s()', $command if $command ;
	}

## Example Base Node Schemas
#
# package NoSQL::PL2SQL::DBI::Schema::table ;
# use base qw( NoSQL::PL2SQL::DBI::Schema ) ;
# 
# sub schema {
# 	shift @_ unless ref $_[0] ;
# 	my $self = shift ;
# 
# 	## default table definition
# 	}
# 
# package NoSQL::PL2SQL::DBI::Schema::index ;
# use base qw( NoSQL::PL2SQL::DBI::Schema ) ;
# 
# sub schema {
# 	shift @_ unless ref $_[0] ;
# 	my $self = shift ;
# 
# 	## default index definition
# 	}
# 
1;
__END__
# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

NoSQL::PL2SQL::DBI - Base Perl RDB driver for NoSQL::PL2SQL

=head1 SYNOPSIS

  package NoSQL::PL2SQL::DBI::123SQL ;
  use base qw( NoSQL::PL2SQL::DBI ) ;

  package MyArbitraryClass ;
  use base qw( NoSQL::PL2SQL ) ;
  use NoSQL::PL2SQL::DBI::123SQL ;

  ## Primary User Methods

  my $dsn = new NoSQL::PL2SQL::DBI::123SQL $tablename ;
  $dsn->connect( $data_source, $username, $auth, \%attr ) ;
  $dsn->do('DROP TABLE %s') ;
  $dsn->loadschema ;

  ## Internally Used Methods
  
  my @nvp = ( [ $name, $value, $isstring ], ... ) ;
  my $perldata = $dsn->fetch( @nvp ) ;
  my %results = $dsn->insert( @nvp ) ;
  my %results = $dsn->update( $recordid, @nvp ) ;
  $dsn->delete( $recordid ) ;

  my $encoded = $dsn->encodestring( $text ) ;
  my $recno = $dsn->lastinsert ;
  my @sql = $dsn->schema ;
  
  ## Utilities and debugging

  $dsn->sqldump( $reset = 1 ) ;
  $dsn->debug( $arbitrarystring ) ;
  print join "\n", $dsn->sqldump() ;

  my @fetchrows = $dsn->rows_hash('SELECT * FROM %s WHERE objectid=1') ;
  my @fetchrows = $dsn->rows_array('SELECT * FROM %s WHERE objectid=1') ;

  my $sql = $dsn->sqlstatement( $sqlarg ) ;
  my $db = $dsn->db ;
  $tablename = $dsn->table ;

=head1 DESCRIPTION

NoSQL::PL2SQL::DBI provides an abstraction for connecting to an external database.  Subclass definitions should be used for specific implementations.  These methods may be generally useful, but are intended for use with NoSQL::PL2SQL, an object backing mechanism.

An end user implementing NoSQL::PL2SQL, or any class which implements this package, will access this driver using the constructor and the 3 methods listed above as "Primary User Methods".

Anyone implementing a subclass needs to understand the "Internally Used Methods" above.

Developers who are comfortable with RDB can design a thin object interface using any number of tools, such as DBIx::Class.  NoSQL::PL2SQL is designed for developers of thicker objects that may be more logical and require data flexibility.  For these developers, where the database is merely a mechanism for object persistance, NoSQL::PL2SQL provides a simple abstraction with a trivial interface, and great portability.

One of NoSQL::PL2SQL's features is a "universal" table definition that can accomodate arbitrary and indeterminate data structures.  This flexibility means that a single table can be used for heterogeneous instantiations of different classes.  In many cases, a single table can serve the data needs of an entire application.  Consequently, a NoSQL::PL2SQL::DBI object is primarily defined by the tablename using a constructor argument.

A NoSQL::PL2SQL:DBI instance consists of one other property, a database handle.  This handle is defined using the C<connect()> method with the same arguments as the default C<< DBI->connect() >> method.  Otherwise, the default handle is a NoSQL::PL2SQL::DBI::Null object that simply reflects statement arguments, and can be useful for debugging.

The NoSQL::PL2SQL::DBI AUTOLOAD overrides any DBI method.  Because the RDB table is abstracted within, SQL statements do not need to specify a table.  The C<sprintf()> notation is used instead- replacing '%s' in any SQL construction with the table name first.  The C<sqlstatement()> method is always used for this translation.

Additionally, NoSQL::PL2SQL::DBI provides versions of C<< DBI->fetchrow_arrayref() >> and C<< DBI->fetchrow_hashref >>- C<rows_array()> and C<rows_hash()> respectively.  These methods take an SQL statement as an argument, perform preparation and execution, and return the same output as their counterparts.

C<perldata()> is nearly the same as C<rows_hash()>, except the output is a hash reference that keys each record on its recordid.  Originally, the hashref was blessed as a NoSQL::PL2SQL::Perldata object, hence the name.  All NoSQL::PL2SQL data structures are implemented as a tree of nodes.  And the static methods in NoSQL::PL2SQL::Perldata are used to access the RDB records as though they were tree nodes.  

All RDB inquiries made by NoSQL::PL2SQL expect a hashref structure similar to C<perldata()>'s.  As of v1.0, only the C<fetch()> method is used.  Additionally, NoSQL::PL2SQL passes its requests as a list of NVP's (name value pairs).  The nvp arguments are arrayrefs consisting of a string name, a scalar value, and a boolean that identifies the value as a string.  The boolean argument controls the SQL construction and triggers encoding, via C<stringencode()>.  C<delete()>, which takes a recordid as an argument, is the only exception.

The C<insert()> method is trivial.  Implementations only need to override the C<update()> method.  C<insert()> needs to return a recordid value, which is determined by the underlying RDB application.  Both C<insert()> and C<update()> return NVP's as a hash reference containing an element named "id".  The other element, "sqlresults", contains the only useful output when the connected database is the default "NoSQL::PL2SQL::DBI::Null".

The following methods are implemented, by default, to use an SQL syntax compatible with MySQL and SQLite.  Other RDB applications may require overriding these methods:

=over 8

=item C<fetch()>

=item C<update()>

=item C<delete()>

=item C<lastinsertid()>

=item C<stringencode()>

=back

=head1 SCHEMA

The purpose of the schema is to build a data source that conforms to the NVP arguments of the above methods.  The C<loadschema()> method triggers the build.  So implementations that override C<loadschema()> can ignore the specification below.  However, database applications that use SQL as an interface should be implemented consistently.

In NoSQL::PL2SQL::DBI and its implementations, the C<schema()> method should return one or more SQL directives.  The default C<loadschema()> feeds each into C<< NoSQL::PL2SQL::DBI->do() >>.  Consequently, the SQL statements should always refer to the table name as '%s'.  C<< NoSQL::PL2SQL::DBI->schema() >> takes no argument.  Instead, it uses an internal definition.  The default definition, designed for MySQL, has an XML format using an ad-hoc XML definition.  This definition may be replaced with a more universal standard, or hopefully prove to be suitably extensible.

There are two default C<schema()> definitions.  The first, C<< NoSQL::PL2SQL::DBI->schema() >>, converts the XML definition into an XML::Parser::Nodes tree.  This tree is reblessed into another package as follows:

  return bless( $nodes, ref( $dsn ) .'::Schema' )->schema() ;

Consequently, there is a second default schema called C<< NoSQL::PL2SQL::DBI::Schema->schema() >>, (For convenience, these two will be distinguished as C<schema()> and C<< Schema->schema() >>.)  An implementation must be defined as follows, using 123SQL as an example implementation.

  package NoSQL::PL2SQL::DBI::123SQL ;
  use base qw( NoSQL::PL2SQL::DBI ) ;

  package NoSQL::PL2SQL::DBI::123SQL::Schema ;
  use base qw( NoSQL::PL2SQL::DBI::Schema ) ;

By default, C<< Schema->schema() >> calls the schema method on its child nodes.  For example, each SQL statement is represented by an <sql> node.  In order to return an SQL statement, the following must be defined (using the same example):

  package NoSQL::PL2SQL::DBI::123SQL::Schema::sql ;
  use base qw( NoSQL::PL2SQL::DBI::Schema ) ;

This definition, however, is only required for explict SQL output.  Otherwise, the default C<< Schema->schema() >> method is called in recursion on the next level of child nodes.  The nodes below are shown as XML and with defined methods:

  ## <table command="CREATE" ...>
  ##   <column ... />
  ## </table>

  package NoSQL::PL2SQL::DBI::123SQL::Schema::table ;
  use base qw( NoSQL::PL2SQL::DBI::Schema ) ;

  sub schema {
	my $self = shift ;
	return $self->command ;
	}

  sub CREATE {
	my $self = shift ;
	my @columns = NoSQL::PL2SQL::DBI::Schema->schema( $self ) ;
	## combine columns into a single SQL directive
	}

  package NoSQL::PL2SQL::DBI::123SQL::Schema::table::column ;
  use base qw( NoSQL::PL2SQL::DBI::Schema ) ;

  sub schema {
	my $self = shift ;
	## return column definition
	}

The XML node shown above, named table, is processed by C<< Schema->schema() >>, and its explicitly defined C<< Schema::table->schema() >> method is called.  That method punts to another method, defined by the "command" attribute of the node, and the << Schema::table->CREATE() >> method is called in turn.  That method gets its child schemas by calling the default << Schema->schema() >> method.  At this point, the package names of the child schemas start accumulating, and each of those C<schema()> methods return substrings that are combined into a single SQL directive.

To summarize, a schema definition requires the definition of a number of package classes.  The package names correlate to the structure of the node tree (see XML::Parser::Nodes::tree()).  Each package class needs to extend C<NoSQL::PL2SQL::DBI::Schema>, and may or may not override the C<schema()> method.  Output can be varied by defining methods that correspond to the "command" attribute.

In general, there's probably no need to define a package unless the C<schema()> method will be overridden.  But consider the following definitions:

  package NoSQL::PL2SQL::DBI::MySQL::Schema ;	## The Schema
  use base qw( NoSQL::PL2SQL::DBI::Schema ) ;	## The Base Schema

  package NoSQL::PL2SQL::DBI::MySQL::Schema::table ;	## A Node Schema
  use base qw( NoSQL::PL2SQL::DBI::Schema ) ;	## The Base Schema

  ## Not defined but part of the model
  package NoSQL::PL2SQL::DBI::Schema::table ;	## A Base Node Schema

For undefined packages, the inheritance order is:

=over 8 

=item Base Node Schema >> Schema >> Base Schema

=back

A package may be defined without an overriding C<schema()> definition in order to define a different inheritance.

=head2 EXPORT

None by default.


=head1 HISTORY

=over 8

=item 0.01

Original version; created by h2xs 1.23 with options

  -AXCO
	NoSQL::PL2SQL

=item 0.02

Cleaned perldoc formatting issues

Added optional arg to C<schema()> method

=item 0.03

Added optional arg to C<schema()> method

=item 0.04

Added C<debug()> method

=back


=head1 SEE ALSO

=over 8

=item NoSQL::PL2SQL

=item XML::Parser::Nodes

=item http://pl2sql.tqis.com/

=back


=head1 AUTHOR

Jim Schueler, E<lt>jim@tqis.comE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2012 by Jim Schueler

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.9 or,
at your option, any later version of Perl 5 you may have available.


=cut

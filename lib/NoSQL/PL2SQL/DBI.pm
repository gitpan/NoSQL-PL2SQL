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

our $VERSION = '0.11';

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
    <index name="%s_reference" command="CREATE" table="%s">
      <column name="objectid" />
      <column name="objecttype" />
      <column name="reftype" />
    </index>
  </sql>
</mysql>
endschema

my $indexschema =<<'endschema' ;
<mysql>
  <sql>
    <table command="CREATE" table="%s">
      <column name="textkey" type="INT" />
      <column name="intkey" type="INT" />
      <column name="datekey" type="INT" />
      <column name="textvalue" type="VARCHAR" length="128" />
      <column name="intvalue" type="INT" />
      <column name="datevalue" type="DATE" />
      <column name="objectid" type="INT" />
    </table>
  </sql>
  <sql>
    <index name="%s_all" command="CREATE" table="%s">
      <column name="textkey" />
      <column name="intkey" />
      <column name="datekey" />
      <column name="textvalue" />
      <column name="intvalue" />
      <column name="datevalue" />
      <column name="objectid" />
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

## indexschema is used by NoSQL::PL2SQL::Simple.  No one has expressed
## an intention of another DBI implementation.  Since I can get away with 
## it, I'm arbitrarily extending this definition, and I really shouldn't.
## In the future apps, will have to create their own database specific 
## subclasses:  NoSQL::PL2SQL::Simple::MySQL, etc.

sub indexschema {
	return $indexschema ;
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

	my $ct = 0 ;
	$ct++ while $sprintf =~ /%s/g ;
	return sprintf $sprintf, ( $self->[1] ) x$ct ;
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

sub NoSQL::PL2SQL::DBI::perldata::perldata {
	my $ary = shift ;
	return {} if @$ary == 0 || ! exists $ary->[0]->{id} ;
	my %r = map { $_->{id} => $_ } @$ary ;
	return \%r ;
	}

sub new {
	my $package = shift ;
	my $tablename = shift ;
	return bless [ $nulldbi, $tablename ], $package ;
	}

sub connect {
	my $self = shift ;
	my $ref = $self ;
	$ref = $ref->[0] while ref $ref eq ref $self
				&& ref $ref->[0] eq ref $self ;
	$ref->[0] = DBI->connect( @_ ) ;
	return $self ;
	}

sub table {
	my $self = shift ;
	return $self->[1] unless @_ ;

	my $package = ref $self ;
	my $out = $package->new( @_ ) ;
	$out->[0] = $self ;
	return $out ;
	}

sub db {
	my $self = shift ;
	return ref $self->[0] eq ref $self? $self->[0]->db: $self->[0] ;
	}

sub dbconnected {
	my $self = shift ;
	my $db = $self->db ;
	my $unconnected = $db->isa('SCALAR') && $$db eq ref $db ;
	return ! $unconnected ;
	}

## Implementation Specific
## optionally pass a scalar integer- otherwise same arguments as fetch()
sub delete {
	my $self = shift ;
	my @delete = ref $_[0]? @_: ( [ id => $_[0] ] ) ;
	return $self->fetch( 'DELETE FROM %s WHERE', @delete ) ;
	}

## Implementation Specific
sub lastinsertid {
	my $self = shift ;
	my $db = $self->db ;
	return ! $self->dbconnected? 0:
			$db->last_insert_id( 
			  undef, undef, $self->table, 'id' ) ;
	}

###############################################################################
##
##  A special case of update() to handle two sets of NVP's; e.g
##    where @values = ( [ objectid => $newid ] )
##    and @conditions = ( [ objectid => $oldid ] )
##  $nvp = NoSQL::PL2SQL::DBI->new('')->update( undef => @values )->{nvp} ;
##  $dsn->sqlupdate( $nvp, @conditions ) ;
##
###############################################################################

sub sqlupdate {
	my $self = shift ;
	my $nvp = shift ;
	my $sql = sprintf 'UPDATE %s SET %s WHERE', '%s', $nvp ;
	return $self->fetch( $sql, @_ ) ;
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
			sqlresults => $sqlresults,
			nvp => $nvp
			} ;
	}

sub insert {
	my $self = shift ;
	return $self->update( undef, @_ ) ;
	}

## Implementation Specific.  Default method is MySQL syntax.
sub fetch {
	my $self = shift ;
	my $delete = ( @_ && ! ref $_[0] )? shift( @_ ): undef ;

	my @pairsf = ( '%s=%s', '%s="%s"', '%s=NULL' ) ;
	my $nvp = join ' AND ', map {
			sprintf $pairsf[ defined $_->[1]? 
			  $_->[2] || length $_->[1] == 0: 2 ],
			$_->[0], $self->stringencode( $_->[1], ! $_->[2] )
			} @_ ;

	my $sql = join ' ', $delete || 'SELECT * FROM %s WHERE', $nvp ;
	return $self->do( $sql ) if defined $delete || ! $self->dbconnected ;
	my @out = $self->rows_hash( $sql ) ;
	return wantarray? @out: bless \@out, __PACKAGE__ .'::perldata' ;
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
  my $perldata = $dsn->fetch( @nvp )->perldata ;
  my %results = $dsn->insert( @nvp ) ;
  my %results = $dsn->update( $recordid, @nvp ) ;
  $dsn->delete( $recordid ) ;
  $dsn->delete( @nvp ) ;

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

C<rows_array()> and C<rows_hash()> may be used as a convenience.  However, these methods required syntactically appropriate SQL instead of something independent of the underlying database.  The C<fetch()> method should be used instead of C<rows_hash()>.

If the output is piped into the C<perldata()> method, C<< fetch()->perldata >>, the results are a set of NVP's keyed in the recordid.  All NoSQL::PL2SQL data structures are implemented as a tree of nodes.  And each NVP (originally blessed as I<NoSQL::PL2SQL::Perldata>) represents a node.  In the NoSQL::PL2SQL::Perldata class, unblessed nodes are passed to static methods.

To ensure SQL independence, C<NoSQL::PL2SQL::DBI> methods are called using a set of nvp arguments: Each arguments is an arrayref consisting of a string name, a scalar value, and a boolean to distinguish string values.  The boolean argument controls the SQL construction and triggers encoding, via C<stringencode()>.  

C<delete( $id )> shows a single scalar argument which is understood to mean C<< delete( [ id => $id ] )>>.

The C<insert()> method is trivial.  Implementations only need to override the C<update()> method.  C<insert()> needs to return a recordid value, which is determined by the underlying RDB application.  Both C<insert()> and C<update()> return NVP's as a hash reference containing an element named "id".  The other element, "sqlresults", contains the only useful output when the connected database is the default "NoSQL::PL2SQL::DBI::Null".

The following methods are implemented, by default, to use an SQL syntax compatible with MySQL and SQLite.  Other RDB applications may require overriding these methods:

=over 8

=item C<fetch()>

=item C<update()>

=item C<sqlupdate()>

=item C<delete()>

=item C<lastinsertid()>

=item C<indexschema()>

=item C<stringencode()>

=back

=head1 GENERAL USE

As of version 0.10, features have been added to make the DBI more useful for schemas other than the default.  For example, delete now takes conditional arguments similar to C<fetch()>:

  $dsn->delete( [ textkey => 20 ] ) ;

A more complicated example involves an update request, which requires two sets of nvp's:  The first defines the values and the second defines a conditional.  Since C<update()> is a variation of C<insert()> and accepts no conditional arguments, use C<sqlupdate()> instead:

  my @values = ( [ refto => 20 ] ) ;
  my @conditions = ( [ objectid => 1 ] ) ;

  my $dsn = NoSQL::PL2SQL::DBI->new('mytable') ;	## unconnected
  print $dsn->fetch( @conditions ) ;		## pass conditional nvp's
  print $dsn->insert( @values )->{sqlresults} ;	## pass value nvp's

  ## Generate the values clause as a separate string
  my $nvp = NoSQL::PL2SQL::DBI->new('')->insert( @values )->{nvp} ;

  ## Pass that values clause into the sqlupdate() method
  print $dsn->sqlupdate( $nvp, @conditions ) ;

  ## Prints: UPDATE mytable SET refto=20 WHERE objectid=1

Implementations of PL2SQL::DBI handle slight variations of SQL.  And the translation instructions are distributed among various methods somewhat arbitrarily:  The command syntax is defined in sqlupdate(); the values clause syntax is defined in update(); and the conditional clause is defined in fetch().

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

The XML node shown above, named table, is processed by C<< Schema->schema() >>, and its explicitly defined C<< Schema::table->schema() >> method is called.  That method punts to another method, defined by the "command" attribute of the node, and the C<< Schema::table->CREATE() >> method is called in turn.  That method gets its child schemas by calling the default C<< Schema->schema() >> method.  At this point, the package names of the child schemas start accumulating, and each of those C<schema()> methods return substrings that are combined into a single SQL directive.

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

=item 0.05

Generalized C<fetch()> and C<perldata()> methods to handle arbitrary schemas

C<perldata()> arguments are explicitly defined

C<delete()> now accepts the same arguments as C<fetch()>

With an argument C<table()> creates a second DSN instance chained via C<db()>.

=item 0.10

Added C<sqlupdate()>.

Added nvp element to C<update()>'s return value.

Fixed a bug in the $xmlschema "CREATE INDEX" node

Modified C<sqlstatement()>

Added C<indexschema()> for NoSQL::PL2SQL:Simple

=item 0.11

C<perldata()> now B<always> returns a hash ref and C<fetch()> B<always> returns an array.  In order to combine duplicated functionality, C<perldata()> is now invoked as C<< $dsn->fetch()->perldata >>.

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

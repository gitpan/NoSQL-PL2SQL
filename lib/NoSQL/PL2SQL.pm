package NoSQL::PL2SQL;

use 5.008009;
use strict;
use warnings;

use Scalar::Util ;
use Carp ;
use NoSQL::PL2SQL::Node ;
use NoSQL::PL2SQL::Object ;
use NoSQL::PL2SQL::Perldata ;

require Exporter;

our @ISA = qw(Exporter);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration	use NoSQL::PL2SQL ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
our %EXPORT_TAGS = ( 'all' => [ qw() ] ) ;

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } ) ;

our @EXPORT = qw() ;

our $VERSION = '1.01';

require XSLoader;
XSLoader::load('NoSQL::PL2SQL', $VERSION);

# Preloaded methods go here.

our @members = qw( perldata sqltable globals ) ;

sub SQLObjectID {
	return sqlobjectid( @_ ) ;
	}

sub sqlobjectid {
	my $self = shift ;
	return NoSQL::PL2SQL::Object::item( $self )->[1]->record->{objectid} ;
	}

sub SQLObject {
	return sqlobject( @_ ) ;
	}

sub sqlobject {
	my $package = shift ;
	my $dsn = shift ;
	my $objectid = @_ && ! ref $_[0]? shift( @_ ): undef ;
	my $object = @_ && ref $_[0]? shift( @_ ): undef ;

	return carp( "SQLObject must be called as a static method" )
			if ref $package ;
	return carp( 'Missing or invalid data source' ) 
			unless eval { $dsn->db } ;
	return carp( 'Fetch requires an objectid' )
			unless defined $objectid || defined $object ;
	return carp( 'SQLObject requires a connected database. ' 
			.'Use NoSQL::PL2SQL::Node::factory for testing.' )
			unless $dsn->dbconnected ;

	## write to database
	$objectid = NoSQL::PL2SQL::Node->factory( $dsn, $objectid, 
			bless( $object, $package ), $package )
			if defined $object ;

	my $perlnode = $objectid ;

	my $self = bless {}, 'NoSQL::PL2SQL::Object' ;
	$self->{sqltable} = $dsn ;
	$self->{perldata} = 
			## hardcoded arguments for now
			$dsn->fetch( [ objectid => $objectid, 0 ],
			[ objecttype => $package, 1 ] ) ;
	return carp( sprintf "Object not found for object $objectid." )
			  || 0 unless scalar values %{ $self->{perldata} } ;

	## find perldata node if necessary
	my $pdrecord = $self->record( $objectid ) || { id => 0 } ;
	( $perlnode ) = map { $_->{id} } 
			  grep $_->{reftype} eq 'perldata',
			  values %{ $self->{perldata} }
			unless $pdrecord->{id} == $objectid ;
	return warn( 'Missing perldata node- possible data corruption.' )
			unless $perlnode ;

	$self->{top} = $self->record( $perlnode )->{refto} ;
	$self->{package} = $package ;
	$self->{reftype} = $self->record->{reftype} ;
	$self->{globals} = { memory => {}, 
			scalarrefs => {},
			top => $self->{top},
			} ;

	if ( $self->{reftype} eq 'hashref' ) {
		tie my( %out ), $self ;
		return $self->memorymap( $self->mybless( \%out ) ) ;
		}
	elsif ( $self->{reftype} eq 'arrayref' ) {
		tie my( @out ), $self ;
		return $self->memorymap( $self->mybless( \@out ) ) ;
		}
	elsif ( $self->{reftype} eq 'scalarref' ) {
		$self->loadscalarref( $self->{top} ) ;
		tie my( $out ), $self ;
		return $self->memorymap( $self->mybless( \$out ) ) ;
		}
	else {
		return $self->sqlclone ;
		}
	}

sub SQLClone {
	return sqlclone( @_ ) ;
	}

sub sqlclone {
	my $self = shift ;
	$self = $self->sqlobject( @_ ) if @_ >= 2 ;
	return $self unless ref $self ;

	return NoSQL::PL2SQL::Object::item( $self )->[1]->sqlclone ;
	}

sub SQLRollback {
	return sqlrollback( @_ ) ;
	}

sub sqlrollback {
	my $self = shift ;
	my $o = NoSQL::PL2SQL::Object::item( $self )->[1] ;
	$o->{globals}->{rollback} = 1 ;
	}

1;
__END__
# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

NoSQL::PL2SQL - Relational Database Persistence for Perl Objects

=head1 SYNOPSIS

NoSQL::PL2SQL is intended for class designers.  An example of a class that implements NoSQL::PL2SQL, MyArbitraryClass, is defined below.

  package MyArbitraryClass ;
  use base qw( NoSQL::PL2SQL ) ;

  ## define a data source
  use NoSQL::PL2SQL::DBI::SQLite ;
  my $tablename = 'objectdata' ;
  my $dsn = new NoSQL::PL2SQL::DBI::SQLite $tablename ;
  $dsn->connect( 'dbi:SQLite:dbname=:memory:', '', '') ;

  # Preloaded methods go here.

  sub new {
	my $package = shift ;
	my $object = @_? shift( @_ ): {} ;
	my $self = $package->SQLObject( $dsn, $object ) ;
	printf "userid: %d\n", $self->SQLObjectID if ref $object ;
	return $self ;
	}  

  sub error {
	my $self = shift ;
	$self->SQLRollBack ;
	warn "unrecoverable error" ;
	}

  sub clone {
	my $self = shift ;
	return $self->SQLClone ;
	}

  ## Should not be necessary
  END {
	## Destroy all instantiations
	}

The main requirement is that the class inherit NoSQL::PL2SQL methods.  Second, a data source needs to be defined.  One of NoSQL::PL2SQL's features is a "universal" data structure that can accomodate heterogenous, complex data structures.  As a result the table that defines the data source can be shared among different classes.

NoSQL::PL2SQL::DBI contains access methods used by NoSQL::PL2SQL.  In addition to the shown constructor and connector, C<< $dsn->loadschema >> is used to build the datasource table when an application is installed.

NoSQL::PL2SQL's interface is intended to be built into an implementing class's constructor, generally in place of the C<bless> statement.  In this example, the C<< AnyArbitraryClass->new() >> constructor can be invoked three ways.  First, with no arguments, the constructor returns an empty persistent blessed hash reference.  Second, if the single argument is a hash reference, the constructor converts the structure into a persistent blessed object.  Third, if the argument is valid, numeric ObjectID, the constructor returns the stored object that exactly matches the state when it was previously destroyed.

In this example, when a persistent object is initialized, its ObjectID is printed out.  Naturally, this is a clumsy way to maintain object references, although personally, I am not above hardcoding a reference into an HTML document.

Another option is to use a fixed object as an index to other objects.  Unfortunately, as of this writing, NoSQL::PL2SQL has no features for object locking, and this strategy would quickly foul up in a multi-user environment.  A third option is to define another data source to map the ObjectID to another key, such as a user's email address.  A fourth, more complicated example is shown below.

Objects are automatically written when they are destroyed.  This feature can be disabled by calling C<SQLRollback()>.  Another solution is to create an untied cloned object, using C<SQLClone().  Modifications to the clone are destroyed along with the object.

Results have been erratic and unsatisfactory when object destruction is postponed until global destruction- althought experienced programmers will always scope the objects they use.  If a particularly robust solution is required, maintain a univeral list of all instantiations and explicitly destroy each one using an C<END{}> clause, as shown.

=head1 DESCRIPTION

Apparently, many programmers, when envisioning a new project, think about database schemas and imagine the SQL statements that join everything together.  Well into the design, they implement a thin object, using one of the numerous solutions available, to create an OO interface for their application.

I say "apparently" and "many" because I am not among them.  When I envision a new project, I think about classes, interfaces, and imagine lines of inheritance.  Well into the prototype, I am still using native marshalling mechanisms, and when the time comes to add robust and portable persistence, I consider the database design a nuisance.

There are fewer tools for programmers like me-  Although some of the NoSQL initiatives are starting to attract attention.  This design started with some specific objectives to meet my needs, and a few additional features added along the way.

1.  Most importantly, the interface needs to be simple and unobtrusive.
2.  The data implementation needs to be flexible and portable- intended to be rolled out on any shared server that provides MySQL.
3.  The implementation should be relatively lightweight, fast, and minimize resources.

The interface is intended to be a drop-in replacement for Perl's bless operator.  The SQLObject method returns a blessed object that is automatically tied to NoSQL::PL2SQL's RDB persistance.

=head1  AN EXAMPLE USING A STRING KEY

As part of the design objectives, an application should be able to migrate its data to to NoSQL::PL2SQL.  Primarily, I need to specify the object ids that key each object.  (Did I mention these are hardcoded in my HTML?)  In order to specify an object id, see the example below.

When specifying object ids, care should be taken to ensure that each id is unique.  For that reason, an application should consistently assign ids, or always use automatic assignments.  For practicality, this uniqueness constraint only applies to objects in a given class.  In other words, the key definition is a combination of the class name and object id.  Since the class name is a string, the schema can be manipulated to use string keys instead of numerics.  The MyArbitraryClass is redefined below to illustrate this approach.

  ## SQLObject creation using a specified object id
  # MyArbitraryClass->SQLObject( $dsn, $objectid => $dataobject ) ;

  package MyArbitraryClass ;
  use base qw( NoSQL::PL2SQL ) ;

  sub new {
	my $package = shift ;

	return warn unless @_ ;
	my $object = shift @_ ;	## a user record or email scalar

	my $self = ref $object? 
			NoSQL::PL2SQL::SQLObject( 
			  $object->{email}, $dsn, 0, $object ):
			NoSQL::PL2SQL::SQLObject( $object, $dsn, 0 ) ;
	return bless $self, $package ;
	}  

The constructor is called with a single argument that is either the email address as scalar string, or a user record where the email address is an element called "email".  With this approach, the email address masquerades as a class name.  The object id is not accessible to the user, so it should consistently be set to a value such as 0.  Using this strategy, a user record is accessed whenever a user enters an email address.


=head1 ARCHITECTURE

XML::Dumper::pl2xml provides the basic mechanism for converting an arbitrary complex Perl data structure into an XML tree.  XML::Parser::Nodes treats this tree as a set of identical nodes.  Originally, NoSQL::PL2SQL::Node simply extended this module by adding a method to write itself as a RDB table record.  The name PL2SQL is a reference to this legacy.

Hopefully, users will infer PL2SQL also include SQL2PL functionality, so implementing objects can move back and forth between persistent storage and active use.  The approach is to retain the node tree structure, and use Perl TIE magic to make the node containers appear as the original object members.  The SQL2PL functionality is embodied in the NoSQL::PL2SQL::Object package, which defines the TIE constructors and all overloading methods.

NoSQL::PL2SQL::DBI defines a data abstraction for accessing the data sources.  The raw RDB record data is accessed using methods in the NoSQL::PL2SQL::Perldata package.  Each of the modules contains detailed information about their architecture and internal operations.


=head2 EXPORT

None by default.



=head1 SEE ALSO

=over 8

=item XML::Parser::Nodes

=item NoSQL::PL2SQL::DBI

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

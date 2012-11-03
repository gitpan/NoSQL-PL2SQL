# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl NoSQL-PL2SQL.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test::More tests => 52 ;

BEGIN { 
	use_ok('Scalar::Util') ;
	use_ok('XML::Parser::Nodes') ;
	use_ok('NoSQL::PL2SQL::Node') ;
	use_ok('NoSQL::PL2SQL::Perldata') ;
	use_ok('NoSQL::PL2SQL::Object') ;
	use_ok('NoSQL::PL2SQL') ;

	use_ok('NoSQL::PL2SQL::DBI::SQLite') ;
	};

#########################

use Storable qw( freeze thaw store retrieve ) ;
use Digest::MD5 ;
use Data::Dumper ;

my $rebuild = 1 ;
my $adjust = 0 ;
my @retr = () ;
my @rowct = 0 ;
my $assignedid = 0 ;

my $tablename = 'objectdata' ;
my $dsn = new NoSQL::PL2SQL::DBI::SQLite $tablename ;

is( @$dsn, 2 ) ;
is( $dsn->table, $tablename ) ;
is( ref $dsn->db, 'NoSQL::PL2SQL::DBI::Null' ) ;
is( $dsn->lastinsertid, 0 ) ;
is( my @ct = $dsn->schema, 2 ) ; 	## MySQL

sub objectvalue {
	my $o = shift ;
	my $scalar = Storable::freeze( $o ) ;
	return Digest::MD5::md5_hex( $scalar ) ;
	}

# my @rowct = $dsn->rows_array('SELECT COUNT(*) FROM %s') ;
# is( $rowct[0][0], 0 ) ;


# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

is( NoSQL::PL2SQL::Node::typeis( 1 ), 'integer' ) ;
is( NoSQL::PL2SQL::Node::typeis( 1.0 ), 'double' ) ;
is( NoSQL::PL2SQL::Node::typeis( "1" ), 'string' ) ;

# @TestRequest::ISA = qw( NoSQL::PL2SQL ) ;
do {
	package TestRequest ;
	use base qw( NoSQL::PL2SQL ) ;
	} ;

my $request = bless {
    'QBMSXML' => {
        'MsgsRq' => [ 
            {
                'CreditCard' => {
                    'Amount' => '10.00',
                    'Year' => '2012',
                    'Number' => '4111111111111111',
                    'RequestID' => '546696356386',
                    'Month' => '12',
                    'CardPresent' => 'false'
                    }
                },
            {
                'CreditCard' => {
                    'Amount' => '20.00',
                    'Year' => '2014',
                    'Number' => '4123111111111111',
                    'RequestID' => '546696356387',
                    'Month' => '8',
                    'CardPresent' => 'false'
                    }
                }
            ],
        'Singon' => {
            'Desktop' => {
                'DateTime' => '2012-02-29T12:40:09',
                'Ticket' => 'gas8p9ee-re2s9old-ref2i6t',
                'Login' => 'tqis.com'
                }
            }
        }
    }, 'TestRequest' ;

my @nodes = NoSQL::PL2SQL::Node->factory( $dsn, $request ) ;
is( scalar @nodes, 33 ) ;

my @combined = NoSQL::PL2SQL::Node->combine( @nodes ) ;
is( scalar @combined, 25 ) ;

## Connect to database
$dsn->connect('dbi:SQLite:dbname=:memory:','','') ;

do {
	$o = TestRequest->sqlobject( $dsn, 0 ) ;
	warn "\n", Dumper( $o ) ;
	exit ;
	} unless $rebuild ;

do {
#	$dsn->do('DROP TABLE %s') ;
	
	## Create datasource
	$dsn->loadschema ;
	
	@rowct = $dsn->rows_array('SELECT COUNT(*) FROM %s') ;
	is( 0, $rowct[0][0] ) ;
	
	my $o = TestRequest->SQLObject( $dsn, $request ) ;
	is( ref $o, ref $request ) ;
	
	$assignedid = $o->SQLObjectID ;
	ok( defined $assignedid ) ;
	} if $rebuild ;

sub testchanges {
	my $fun = shift ;
	my $retr = TestRequest->sqlobject( $dsn => $assignedid ) ;
	map { &$fun( $_ ) } ( $retr, $request ) ;
	undef $retr ;

	$retr = TestRequest->sqlobject( $dsn => $assignedid ) ;
	return ( $retr ) if wantarray ;
	is( objectvalue( $retr->sqlclone ), objectvalue( $request ) ) ;
	}

## Used for debugging
sub funchanges {
warn "\n" ;
	my $fun = shift ;
	my $retr = TestRequest->sqlobject( $dsn => 1 ) ;

	map { &$fun( $_ ) } ( $retr, $request ) ;
$dsn->sqldump(1) ;
	undef $retr ;
warn join "\n", '', $dsn->sqldump ;
	$retr = TestRequest->sqlobject( $dsn => 1 ) ;

#warn join "\n", '', Dumper( $retr->sqlclone ), '', Dumper( $request ) ;
	return ( $retr ) if wantarray ;
	is( objectvalue( $retr->sqlclone ), objectvalue( $request ) ) ;
	}

testchanges( sub {} ) ;


# splice a hashref
testchanges( sub { 
		splice @{ $_[0]->{QBMSXML}->{MsgsRq} }, 1, 0, 
		  bless( { hello => 'world' }, 'gymbag' ) ;
		} ) ;

# change hashref to hashref
testchanges( sub {
		$_[0]->{QBMSXML}->{MsgsRq}->[1] = 
		  bless( {'a'..'n'}, 'gymbag' ) ;
		} ) ;

# change hashref to arrayref
testchanges( sub {
		$_[0]->{QBMSXML}->{MsgsRq}->[1] = 
		  bless( [ 'hello', 'world' ], 'gymbag' ) ;
		} ) ;

# change hashref to string
testchanges( sub {
		$_[0]->{QBMSXML}->{MsgsRq}->[1] = 'fantasy' ;
		} ) ;

# change hashref to integer
testchanges( sub {
		$_[0]->{QBMSXML}->{MsgsRq}->[0] = 20 ;
		} ) ;
 
# push-pop-push
testchanges( sub {
		push @{ $_[0]->{QBMSXML}->{MsgsRq} }, [ 'a'..'e' ] ;
		my $oo = pop @{ $_[0]->{QBMSXML}->{MsgsRq} } ;
		} ) ;

# push-pop-push
testchanges( sub {
		push @{ $_[0]->{QBMSXML}->{MsgsRq} }, [ 1..5 ] ;
		my $oo = pop @{ $_[0]->{QBMSXML}->{MsgsRq} } ;
		push @{ $_[0]->{QBMSXML}->{MsgsRq} }, "mister mystery" ;
		unshift @{ $_[0]->{QBMSXML}->{MsgsRq} }, $oo ;
		} ) ;

# undef
testchanges( sub {
		$_[0]->{QBMSXML}->{Null} = undef ;
		} ) ;

# large scalar
$alpha = join '', ('a'..'z') ;
testchanges( sub {
		$_[0]->{QBMSXML}->{Singon} = $alpha x100 ;
		} ) ;
is ( length $request->{QBMSXML}->{Singon}, 2600 ) ;

# larger scalar
local( *H ) ;
my $fh = *H ;
ok( open( $fh, "$0" ), 'can\'t open text file' ) ;
our $buff = '' ;
do { 
	undef $/ ;
	$buff = <$fh> ;
	} ;

testchanges( sub {
		$_[0]->{QBMSXML}->{Singon} = $buff ;
		} ) ;
is ( $request->{QBMSXML}->{Singon}, $buff ) ;

# smaller scalar
testchanges( sub {
		$_[0]->{QBMSXML}->{Singon} = 'Jim Schueler' ;
		} ) ;

# add another hash
testchanges( sub {
		delete $_[0]->{QBMSXML}->{Singon} ;
		$_[0]->{QBMSXML}->{Singon} =
		  bless( { hello => 'world' }, 'gymbag' ) ;
		} ) ;

# add existing internal reference
@retr = testchanges( sub {
		$_[0]->{QBMSXML}->{Singup} = 
		  bless( { hello => 'tokyo' }, 'gymshoes' ) ;
		$_[0]->{QBMSXML}->{Singup} = $_[0]->{QBMSXML}->{Singon} ;
		} ) ;

$request->{QBMSXML}->{Singup}->{hello} = 'welt' ;
$retr[0]->{QBMSXML}->{Singup}->{hello} = 'welt' ;
is( $retr[0]->{QBMSXML}->{Singon}->{hello}, 'welt' ) ;
@retr = () ;

# delete internal reference
testchanges( sub {
		delete $_[0]->{QBMSXML}->{Singon} ;
		} ) ;

# add new internal reference
@retr = testchanges( sub {
		$_[0]->{QBMSXML}->{SingSong}->[0] = 
				[ qw( do re mi ) ] ;
		push @{ $_[0]->{QBMSXML}->{SingSong} },
				'wabbit',
				$_[0]->{QBMSXML}->{SingSong}->[0] ;
		} ) ;

push @{ $request->{QBMSXML}->{SingSong}->[0] }, 'fa' ;
push @{ $retr[0]->{QBMSXML}->{SingSong}->[0] }, 'fa' ;
is( $retr[0]->{QBMSXML}->{SingSong}->[-1]->[-1], 'fa' ) ;

@retr = testchanges ( sub {
		shift @{ $_[0]->{QBMSXML}->{SingSong} } ;
		} ) ;

@rowct = $dsn->rows_array('SELECT COUNT(*) FROM %s') ;
is( $rowct[0][0], 56 +$adjust ) ;

testchanges( sub {
		$_[0]->{QBMSXML}->{MsgsRq} = [ qw( fee fi fo fum ) ] ;
		} ) ;

@rowct = $dsn->rows_array('SELECT COUNT(*) FROM %s') ;
is( $rowct[0][0], 43 +$adjust ) ;	## should be smaller than 57

@retr = testchanges( sub {
		$_[0]->{QBMSXML}->{Singon} = $_[0]->{QBMSXML}->{Singup} ;
		delete $_[0]->{QBMSXML}->{Singup} ;
		$_[0]->{QBMSXML}->{Singup} = \"magic!" ;
		$_[0]->{QBMSXML}->{Singon} = $_[0]->{QBMSXML}->{Singup} ;
		} ) ;

is( ${ $retr[0]->{QBMSXML}->{Singup} }, ${ $retr[0]->{QBMSXML}->{Singon} } ) ;

my $longstring = $alpha x30 ;
$request->{QBMSXML}->{Singon} = \$longstring ;
${ $retr[0]->{QBMSXML}->{Singup} } = $longstring ;
is( ${ $retr[0]->{QBMSXML}->{Singon} }, ${ $request->{QBMSXML}->{Singon} } ) ;
@retr = () ;

testchanges( sub {
		delete $_[0]->{QBMSXML}->{Singup} ;
		} ) ;

testchanges( sub {
		$_[0]->{QBMSXML}->{MsgsRq}->[2] = 'do' ;
		unshift @{ $_[0]->{QBMSXML}->{MsgsRq} }, 'dee', 'di' ;
		} ) ;

my $iran =<<eof ;
 It is alleged that Iran is ‘four years closer to having a nuclear weapon.’ There is no solid evidence that Iran even has a nuclear weapons program, as opposed to a civilian nuclear enrichment program to produce fuel for electricity-generating plants (the US has 100 of these and generates the fuel for them). If it doesn’t have a nuclear weapons program, it can’t be closer to having a bomb. The question is being begged here, which is a logical fallacy and bad policy.
eof

$assignedid = 2 ;
$request = bless [ 
            {
                'CreditCard' => {
                    'Amount' => '10.00',
                    'Year' => '2012',
                    'Number' => '4111111111111111',
                    'RequestID' => '546696356386',
                    'Month' => '12',
                    'CardPresent' => 'false'
                    }
                },
            {
                'CreditCard' => {
                    'Amount' => '20.00',
                    'Year' => '2014',
                    'Number' => '4123111111111111',
                    'RequestID' => '546696356387',
                    'Month' => '8',
                    'CardPresent' => 'false'
                    }
                },
            $iran x 10,
            ], 'TestRequest' ;

TestRequest->SQLObject( $dsn, $assignedid => $request ) ;
testchanges( sub {} ) ;

$assignedid = 3 ;
my $string = 'NoSQL::PL2SQL' ;
$request = bless \$string, 'TestRequest' ;
TestRequest->SQLObject( $dsn, $assignedid => $request ) ;

testchanges( sub {} ) ;

testchanges( sub {
	${ $_[0] } = 'turquoise' ;
	} ) ;

my @user = () ;

push @user, {
	Name => 'Meg Satellite',
	Email => 'msatellite@gmail.com',
        CreditCard => {
                    'Amount' => '20.00',
                    'Year' => '2014',
                    'Number' => '4123111111111111',
                    'RequestID' => '546696356387',
                    'Month' => '8',
                    'CardPresent' => 'false'
                    },
	Purchases =>  [
		    1351396800,
		    1347595200,
		    1344571200,
		    1339560000,
		    1337400000,
		    ],
	} ;
			

push @user, {
	Name => 'Ruby Oracle',
	Email => 'rroracle@hotmail.com',
        CreditCard => {
                    'Amount' => '20.00',
                    'Year' => '2014',
                    'Number' => '4123111111111111',
                    'RequestID' => '546696356387',
                    'Month' => '8',
                    'CardPresent' => 'false'
                    },
	Purchases =>  [
		    1351396800,
		    1347595200,
		    1344571200,
		    1339560000,
		    1337400000,
		    ],
	} ;
			

push @user, {
	Name => 'Anna McKinley',
	Email => 'mckinley@umich.edu',
        CreditCard => {
                    'Amount' => '20.00',
                    'Year' => '2014',
                    'Number' => '4123111111111111',
                    'RequestID' => '546696356387',
                    'Month' => '8',
                    'CardPresent' => 'false'
                    },
	Purchases =>  [
		    1351396800,
		    1347595200,
		    1344571200,
		    1339560000,
		    1337400000,
		    ],
	} ;
			
my $i = 0 ;
NoSQL::PL2SQL::SQLObject( $user[$i]->{Email}, $dsn, 0 => $user[$i] ) ;

$i++ ;
NoSQL::PL2SQL::SQLObject( $user[$i]->{Email}, $dsn, 0 => $user[$i] ) ;

$i++ ;
NoSQL::PL2SQL::SQLObject( $user[$i]->{Email}, $dsn, 0 => $user[$i] ) ;

sub stringtest {
	my $i = shift ;
	$retr = NoSQL::PL2SQL::SQLObject( $user[$i]->{Email}, $dsn, 0 ) ;
	is( objectvalue( NoSQL::PL2SQL::SQLClone( $retr ) ), 
			objectvalue( $user[$i] ) ) ;
	}

stringtest( 0 ) ;
stringtest( 1 ) ;
stringtest( 2 ) ;

1

package ModelClient::CLI;

use Exporter qw(import);
use Term::ANSIColor;
use Readonly;
use Expect;
use Moose;
use Carp;

our @EXPORT_OK = qw($EOT);
Readonly our $EOT => -1;

has 'client' => ( is => 'ro', isa => 'Expect' );
has 'timeout' => ( is => 'rw', isa => 'Int', default => 8 );

sub BUILDARGS {
    my ( $class, %args ) = @_;

    my $lib_dir = delete $args{lib_dir};
    my $path    = delete $args{path};
    my @params  = @{ delete $args{params} };

    if ($path) {
        local $ENV{'LD_LIBRARY_PATH'} = $lib_dir;
        $args{client} = Expect->spawn( $path, @params )
          or croak colored( "Could not start application $path\n", 'bold red' );
    }

    return \%args;
}

sub BUILD {
    my $self = shift;

    # Make sure, the model is connected
    $self->client->expect( $self->timeout, 'Started/connected' )
      or carp colored(
        "No response from white model, trying to perform test anyway...\n",
        'yellow' );

    return;
}

sub DEMOLISH {
    my $self = shift;

    if ( defined $self->client->pid ) {
        $self->disconnect;
    }

    return;
}

sub disconnect {
    my $self = shift;

    print { $self->client } "quit\n";
    $self->client->expect( $self->timeout, 'Disconnected' )
      or carp colored( "Disconnection failed\n", 'yellow' );

    $self->client->soft_close;

    return;
}

# Wait for application prompt: <0>%
sub expect_prompt {
    my $self = shift;

    $self->client->expect( $self->timeout, '<0>% ' )
      or carp colored( "Prompt not occured\n", 'yellow' );

    return;
}

# Wait for application prompt: <0>%
sub empty_line {
    my $self = shift;

    print { $self->client } "\n";
    $self->expect_prompt;

    return;
}

# Write the value to the register
sub write_value {
    my ( $self, $reg, $val ) = @_;
    print { $self->client } "write $reg $val\n";
    $self->expect_prompt;

    return;
}

# Read the value from register and compare with the expected one
sub read_value {
    my ( $self, $reg ) = @_;

    print { $self->client } "read $reg\n";
    $self->client->expect( $self->timeout, '-re', '\-\> (0x[a-f0-9]+)\s' )
      or croak colored( "Could not read from register $reg\n", 'bold red' );
    $self->expect_prompt;

    return hex $self->client->matchlist->[0];
}

# Send a packet to WM
sub send_packet {
    my ( $self, $packet, $port ) = @_;
    print { $self->client } "send $port @{$packet}\n";
    $self->expect_prompt;

    return;
}

# Receive a packet from WM (port -1 if EOT packet expected), no argument if no validation expected
sub receive_packet {
    my ( $self, $port, @packet ) = @_;

    print { $self->client } "receive raw\n";

    $self->client->expect( $self->timeout, '-re',
        'Received (\d+) .* port (\d+)\s|EOT packet received' )
      or croak colored( "Could not receive a frame\n", 'bold red' );
    $self->expect_prompt;

    my $rx_bytes = $EOT;
    my $rx_port  = $EOT;

    $rx_bytes = int $self->client->matchlist->[0]
      if ( defined $self->client->matchlist->[0] );
    $rx_port = int $self->client->matchlist->[1]
      if ( defined $self->client->matchlist->[1] );

    return ( $rx_port, $rx_bytes );
}

1;

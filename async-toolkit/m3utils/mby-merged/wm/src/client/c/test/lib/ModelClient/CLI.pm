package ModelClient::CLI;

use Term::ANSIColor;
use Expect;
use Moose;
use Carp;

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
    my ( $self, $reg, $val ) = @_;

    print { $self->client } "read $reg\n";
    $self->client->expect( $self->timeout, '-re', '\-\> (0x[a-f0-9]+)\s' )
      or croak colored( "Could not read from register $reg\n", 'bold red' );
    $self->expect_prompt;

    my $output = hex( ( $self->client->matchlist() )[0] );
    ( $output == $val )
      or croak colored( "Expected $val, but got $output\n", 'bold red' );

    print colored( 'Read value correct', 'green' );
    print { $self->client } "\n";
    $self->expect_prompt;

    return;
}

# Send a packet to WM
sub send_packet {
    my ( $self, $port, @packet ) = @_;
    print { $self->client } "send $port @packet\n";
    $self->expect_prompt;
    return;
}

# Receive a packet from WM (port -1 if EOT packet expected), no argument if no validation expected
sub receive_packet {
    my ( $self, $port, @packet ) = @_;

    print { $self->client } "receive raw\n";

    if ( !defined $port || $port >= 0 ) {

        # Receive regular packet
        $self->client->expect( $self->timeout, '-re',
            'Received (\d+) .* port (\d+)\s' )
          or croak colored( "Could not receive a frame\n", 'bold red' );
    }
    else {
        # Receive EOT packet
        $self->client->expect( $self->timeout, 'EOT packet received' )
          or carp colored( "EOT packet expected\n", 'yellow' );
    }

    # Arguments provided, validate the packet
    if ( defined $port && $port >= 0 ) {
        my $rx_port = int( ( $self->client->matchlist() )[1] );
        ( $rx_port == $port )
          or croak colored(
            "Packet received on port $rx_port, but expected on $port\n",
            'bold red' );
    }

    if (@packet) {
        my $bytes = int( ( $self->client->matchlist() )[0] );
        ( $bytes == scalar @packet )
          or croak colored(
"Expected packet size ${\scalar @packet} but received $bytes bytes\n",
            'bold red'
          );
    }

    $self->expect_prompt;
    print colored( 'Received packet correct', 'green' );
    print { $self->client } "\n";
    $self->expect_prompt;

    return;
}
1;

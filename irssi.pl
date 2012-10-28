#!/usr/bin/env perl

use strict;
use Irssi;
use HTML::Entities;
use IO::Socket;
use vars qw($VERSION %IRSSI);

$VERSION = "0.01";

%IRSSI = (
    authors     => 'Daniel Egeberg',
    contact     => 'daniel.egeberg@gmail.com',
    name        => 'im-notify-irssi',
    description => 'Sends notifications to the im-notify server.',
    url         => 'http://github.com/degeberg/im-notify',
);

sub notify {
    my ($server, $nick, $msg) = @_;

    $nick = HTML::Entities::encode($nick);
    $msg = HTML::Entities::encode($msg);

    my $remote = IO::Socket::INET->new(
        Proto    => "tcp",
        PeerAddr => "localhost",
        PeerPort => "1337",
    ) or return;

    print $remote "$nick\n$msg";
    close($remote);
}

sub message_private_notify {
    my ($server, $msg, $nick, $address) = @_;

    return if (!$server);
    notify($server, $nick, $msg);
}

Irssi::signal_add('message private', 'message_private_notify');

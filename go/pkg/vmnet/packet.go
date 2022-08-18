package vmnet

/*
// FIXME: Needed because we call C.send. Perhaps we could use syscall instead?
#include <stdlib.h>
#include <sys/socket.h>

*/
import "C"

import (
	"encoding/binary"
	"io"
	"syscall"

	"github.com/pkg/errors"
)

// Allow Go programs to send and receive raw ethernet frames via vpnkit. See for example dhcp.go.
//
// We support 2 methods for sending ethernet frames to vpnkit:
// 1. via send/recv over a SOCK_DGRAM fd: see ethernetDatagram
// 2. via write/read prefixed with a length over a SOCK_STREAM: see ethernetFramer

// packetReadWriter has the same types as io.ReadWriter but each Read() and Write() operates
// on a whole ethernet frame.
type packetReadWriter = io.ReadWriter

// Datagram sends and receives ethernet frames via send/recv over a SOCK_DGRAM fd.
type Datagram struct {
	Fd   int // Underlying SOCK_DGRAM file descriptor.
	pcap *PcapWriter
}

func (e Datagram) Read(buf []byte) (int, error) {
	num, _, err := syscall.Recvfrom(e.Fd, buf, 0)
	if e.pcap != nil {
		if err := e.pcap.Write(buf[0:num]); err != nil {
			return 0, errors.Wrap(err, "writing to pcap")
		}
	}
	return num, err
}

func (e Datagram) Write(packet []byte) (int, error) {
	if e.pcap != nil {
		if err := e.pcap.Write(packet); err != nil {
			return 0, errors.Wrap(err, "writing to pcap")
		}
	}
	result, err := C.send(C.int(e.Fd), C.CBytes(packet), C.size_t(len(packet)), 0)
	if result == -1 {
		return 0, err
	}
	return len(packet), nil
}

func (e Datagram) Close() error {
	return syscall.Close(e.Fd)
}

var _ packetReadWriter = Datagram{}

// ethernetFramer multiplexes ethernet frames onto a stream, prefixed with a length field.
type ethernetFramer struct {
	rw io.ReadWriter
}

var _ packetReadWriter = ethernetFramer{}

func (e ethernetFramer) Read(buf []byte) (int, error) {
	var len uint16
	if err := binary.Read(e.rw, binary.LittleEndian, &len); err != nil {
		return 0, err
	}
	if err := binary.Read(e.rw, binary.LittleEndian, &buf); err != nil {
		return 0, err
	}
	return int(len), nil
}

func (e ethernetFramer) Write(packet []byte) (int, error) {
	len := uint16(len(packet))
	if err := binary.Write(e.rw, binary.LittleEndian, len); err != nil {
		return 0, err
	}
	if err := binary.Write(e.rw, binary.LittleEndian, packet); err != nil {
		return 0, err
	}
	return int(len), nil
}

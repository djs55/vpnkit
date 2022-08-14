package vmnet

/*
#include <stdlib.h>
#include <sys/socket.h>

*/
import "C"

import (
	"encoding/binary"
	"io"
	"syscall"
)

// Send and receive ethernet frames

type ethernetReadWriter interface {
	Read() ([]byte, error)
	Write(packet []byte) error
}

// ethernetDatagram sends and receives ethernet frames as datagrams.
type ethernetDatagram struct {
	fd  int
	mtu int
}

func (e ethernetDatagram) Read() ([]byte, error) {
	buf := make([]byte, e.mtu)
	num, _, err := syscall.Recvfrom(e.fd, buf, 0)
	return buf[0:num], err
}

func (e ethernetDatagram) Write(packet []byte) error {
	result, err := C.send(C.int(e.fd), C.CBytes(packet), C.size_t(len(packet)), 0)
	if result == -1 {
		return err
	}
	return nil
}

var _ ethernetReadWriter = ethernetDatagram{}

// ethernetStream multiplexes ethernet frames onto a stream.
type ethernetStream struct {
	rw io.ReadWriter
}

var _ ethernetReadWriter = ethernetStream{}

func (e ethernetStream) Read() ([]byte, error) {
	var len uint16
	if err := binary.Read(e.rw, binary.LittleEndian, &len); err != nil {
		return nil, err
	}
	packet := make([]byte, len)
	if err := binary.Read(e.rw, binary.LittleEndian, &packet); err != nil {
		return nil, err
	}
	return packet, nil
}

func (e ethernetStream) Write(packet []byte) error {
	len := uint16(len(packet))
	if err := binary.Write(e.rw, binary.LittleEndian, len); err != nil {
		return err
	}
	if err := binary.Write(e.rw, binary.LittleEndian, packet); err != nil {
		return err
	}
	return nil
}

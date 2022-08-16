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

type packetReadWriter = io.ReadWriter

// ethernetDatagram sends and receives ethernet frames as datagrams.
type ethernetDatagram struct {
	fd int
}

func (e ethernetDatagram) Read(buf []byte) (int, error) {
	num, _, err := syscall.Recvfrom(e.fd, buf, 0)
	return num, err
}

func (e ethernetDatagram) Write(packet []byte) (int, error) {
	result, err := C.send(C.int(e.fd), C.CBytes(packet), C.size_t(len(packet)), 0)
	if result == -1 {
		return 0, err
	}
	return len(packet), nil
}

func (e ethernetDatagram) Close() error {
	return syscall.Close(e.fd)
}

var _ packetReadWriter = ethernetDatagram{}

// ethernetFramer multiplexes ethernet frames onto a stream.
type ethernetFramer struct {
	rw io.ReadWriteCloser
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

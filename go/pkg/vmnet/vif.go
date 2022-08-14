package vmnet

import (
	"encoding/binary"
	"io"
	"net"
)

// Vif represents an Ethernet device
type Vif struct {
	MTU           uint16
	MaxPacketSize uint16
	ClientMAC     net.HardwareAddr
	IP            net.IP
}

func readVif(rw io.ReadWriter) (*Vif, error) {
	var MTU, MaxPacketSize uint16

	if err := binary.Read(rw, binary.LittleEndian, &MTU); err != nil {
		return nil, err
	}
	if err := binary.Read(rw, binary.LittleEndian, &MaxPacketSize); err != nil {
		return nil, err
	}
	var mac [6]byte
	if err := binary.Read(rw, binary.LittleEndian, &mac); err != nil {
		return nil, err
	}
	padding := make([]byte, 1+256-6-2-2)
	if err := binary.Read(rw, binary.LittleEndian, &padding); err != nil {
		return nil, err
	}
	ClientMAC := mac[:]
	var IP net.IP
	return &Vif{MTU, MaxPacketSize, ClientMAC, IP}, nil
}

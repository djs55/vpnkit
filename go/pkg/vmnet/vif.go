package vmnet

import (
	"encoding/binary"
	"io"
	"net"

	"github.com/google/uuid"
)

// Vif represents an Ethernet device
type Vif struct {
	MTU           uint16
	MaxPacketSize uint16
	ClientMAC     net.HardwareAddr
	IP            net.IP
}

func connectVif(rw io.ReadWriter, uuid uuid.UUID) (*Vif, error) {
	e := NewEthernetRequest(uuid, nil)
	if err := e.Write(rw); err != nil {
		return nil, err
	}
	if err := readEthernetResponse(rw); err != nil {
		return nil, err
	}
	vif, err := readVif(rw)
	if err != nil {
		return nil, err
	}
	IP, err := dhcpRequest(rw, vif.ClientMAC)
	if err != nil {
		return nil, err
	}
	vif.IP = IP
	return vif, err
}

// ConnectVifIP returns a connected network interface with the given uuid
// and IP. If the IP is already in use then return an error.
func connectVifIP(rw io.ReadWriter, uuid uuid.UUID, IP net.IP) (*Vif, error) {
	e := NewEthernetRequest(uuid, IP)
	if err := e.Write(rw); err != nil {
		return nil, err
	}
	if err := readEthernetResponse(rw); err != nil {
		return nil, err
	}
	vif, err := readVif(rw)
	if err != nil {
		return nil, err
	}
	vif.IP = IP
	return vif, err
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

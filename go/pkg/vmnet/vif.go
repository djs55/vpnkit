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

func connectVif(conn io.ReadWriter, packet packetReadWriter, uuid uuid.UUID) (*Vif, error) {
	e := NewEthernetRequest(uuid, nil)
	if err := e.Write(conn); err != nil {
		return nil, err
	}
	if err := readEthernetResponse(conn); err != nil {
		return nil, err
	}
	vif, err := readVif(conn)
	if err != nil {
		return nil, err
	}
	IP, err := dhcpRequest(packet, vif.ClientMAC)
	if err != nil {
		return nil, err
	}
	vif.IP = IP
	return vif, err
}

// ConnectVifIP returns a connected network interface with the given uuid
// and IP. If the IP is already in use then return an error.
func connectVifIP(conn io.ReadWriter, uuid uuid.UUID, IP net.IP) (*Vif, error) {
	e := NewEthernetRequest(uuid, IP)
	if err := e.Write(conn); err != nil {
		return nil, err
	}
	if err := readEthernetResponse(conn); err != nil {
		return nil, err
	}
	vif, err := readVif(conn)
	if err != nil {
		return nil, err
	}
	vif.IP = IP
	return vif, err
}

func readVif(conn io.ReadWriter) (*Vif, error) {
	var MTU, MaxPacketSize uint16

	if err := binary.Read(conn, binary.LittleEndian, &MTU); err != nil {
		return nil, err
	}
	if err := binary.Read(conn, binary.LittleEndian, &MaxPacketSize); err != nil {
		return nil, err
	}
	var mac [6]byte
	if err := binary.Read(conn, binary.LittleEndian, &mac); err != nil {
		return nil, err
	}
	padding := make([]byte, 1+256-6-2-2)
	if err := binary.Read(conn, binary.LittleEndian, &padding); err != nil {
		return nil, err
	}
	ClientMAC := mac[:]
	var IP net.IP
	return &Vif{MTU, MaxPacketSize, ClientMAC, IP}, nil
}

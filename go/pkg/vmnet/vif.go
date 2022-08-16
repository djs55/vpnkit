package vmnet

import (
	"encoding/binary"
	"io"
	"net"
	"time"

	"github.com/google/uuid"
	"github.com/moby/vpnkit/go/pkg/vpnkit/log"
)

// Vif represents an Ethernet device
type Vif struct {
	MTU           uint16
	MaxPacketSize uint16
	ClientMAC     net.HardwareAddr
	IP            net.IP
	Packet        packetReadWriter
}

// Fd returns a SOCK_DGRAM which can send and receive raw ethernet frames.
func (v *Vif) Fd() (int, error) {
	d, ok := v.Packet.(ethernetDatagram)
	if ok {
		// return the raw datagram socket
		return d.fd, nil
	}
	// FIXME: make this global in vmnet for easier closing? and better mux/demux
	// create a socketpair and feed one end into the packetReadWriter
	fds, err := socketpair()
	if err != nil {
		return -1, err
	}
	proxy := ethernetDatagram{fds[1]}

	go v.proxy(proxy, v.Packet)
	go v.proxy(v.Packet, proxy)
	return fds[0], nil
}

func (v *Vif) proxy(from, to packetReadWriter) {
	buf := make([]byte, v.MaxPacketSize)
	for {
		n, err := from.Read(buf)
		if err != nil {
			log.Errorf("from.Read: %v", err)
			return
		}
		packet := buf[0:n]
		for {
			_, err := to.Write(packet)
			if err == nil {
				break
			}
			log.Errorf("to.write retrying packet of length %d: %v", len(packet), err)
			time.Sleep(10 * time.Millisecond)
		}
	}
}

func connectVif(conn io.ReadWriter, packet packetReadWriter, uuid uuid.UUID) (*Vif, error) {
	e := NewEthernetRequest(uuid, nil)
	if err := e.Write(conn); err != nil {
		return nil, err
	}
	if err := readEthernetResponse(conn); err != nil {
		return nil, err
	}
	vif, err := readVif(conn, packet)
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
func connectVifIP(conn io.ReadWriter, packet packetReadWriter, uuid uuid.UUID, IP net.IP) (*Vif, error) {
	e := NewEthernetRequest(uuid, IP)
	if err := e.Write(conn); err != nil {
		return nil, err
	}
	if err := readEthernetResponse(conn); err != nil {
		return nil, err
	}
	vif, err := readVif(conn, packet)
	if err != nil {
		return nil, err
	}
	vif.IP = IP
	return vif, err
}

func readVif(conn io.ReadWriter, packet packetReadWriter) (*Vif, error) {
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
	return &Vif{MTU, MaxPacketSize, ClientMAC, IP, packet}, nil
}

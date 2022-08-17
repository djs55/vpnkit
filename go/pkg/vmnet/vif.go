package vmnet

import (
	"bytes"
	"encoding/binary"
	"net"
	"syscall"
	"time"

	"github.com/google/uuid"
	"github.com/moby/vpnkit/go/pkg/vpnkit/log"
	"github.com/pkg/errors"
)

// Vif represents an Ethernet device as a file descriptor.
// Clients should call Fd() and use send/recv to send ethernet frames.
type Vif struct {
	MTU           uint16
	MaxPacketSize uint16
	ClientMAC     net.HardwareAddr
	IP            net.IP
	Ethernet      Datagram // Ethernet allows clients to Read() and Write() raw ethernet frames.
	fds           []int
}

func (v *Vif) Close() error {
	for _, fd := range v.fds {
		_ = syscall.Close(fd)
	}
	return nil
}

// ensure we have a SOCK_DGRAM fd, by starting a proxy if necessary.
func (v *Vif) start(ethernet packetReadWriter) error {
	if _, ok := ethernet.(Datagram); ok {
		// no proxy is required because we already have a datagram socket
		return nil
	}
	// create a socketpair and feed one end into the packetReadWriter
	fds, err := socketpair()
	if err != nil {
		return err
	}
	// remember the fds for Close()
	v.fds = fds[:]
	// client data will be written in this end
	v.Ethernet = Datagram{fds[0]}
	// and then proxied to the underlying packetReadWriter
	proxy := Datagram{fds[1]}
	// proxy until the fds are closed
	go v.proxy(proxy, ethernet)
	go v.proxy(ethernet, proxy)
	return nil
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

func connectVif(fixedSize, ethernet packetReadWriter, uuid uuid.UUID) (*Vif, error) {
	e := NewEthernetRequest(uuid, nil)
	if err := e.Write(fixedSize); err != nil {
		return nil, err
	}
	vif, err := readVif(fixedSize)
	if err != nil {
		return nil, err
	}
	if err := vif.start(ethernet); err != nil {
		return nil, err
	}
	IP, err := dhcpRequest(vif.Ethernet, vif.ClientMAC)
	if err != nil {
		return nil, err
	}
	vif.IP = IP
	return vif, err
}

// ConnectVifIP returns a connected network interface with the given uuid
// and IP. If the IP is already in use then return an error.
func connectVifIP(fixedSize, ethernet packetReadWriter, uuid uuid.UUID, IP net.IP) (*Vif, error) {
	e := NewEthernetRequest(uuid, IP)
	if err := e.Write(fixedSize); err != nil {
		return nil, err
	}
	vif, err := readVif(fixedSize)
	if err != nil {
		return nil, err
	}
	if err := vif.start(ethernet); err != nil {
		return nil, err
	}
	vif.IP = IP
	return vif, err
}

func readVif(fixedSize packetReadWriter) (*Vif, error) {
	// https://github.com/moby/vpnkit/blob/6039eac025e0740e530f2ff11f57d6d990d1c4a1/src/hostnet/vmnet.ml#L160
	buf := make([]byte, 1+1+256)
	if err := binary.Read(fixedSize, binary.LittleEndian, &buf); err != nil {
		return nil, errors.Wrap(err, "reading VIF metadata")
	}
	br := bytes.NewReader(buf)

	var responseType uint8
	if err := binary.Read(br, binary.LittleEndian, &responseType); err != nil {
		return nil, errors.Wrap(err, "reading response type")
	}
	if responseType != 1 {
		var len uint8
		if err := binary.Read(br, binary.LittleEndian, &len); err != nil {
			return nil, errors.Wrap(err, "reading error length")
		}
		message := make([]byte, len)
		if err := binary.Read(br, binary.LittleEndian, &message); err != nil {
			return nil, errors.Wrap(err, "reading error message")
		}
		return nil, errors.New(string(message))
	}

	var MTU, MaxPacketSize uint16
	if err := binary.Read(br, binary.LittleEndian, &MTU); err != nil {
		return nil, err
	}
	if err := binary.Read(br, binary.LittleEndian, &MaxPacketSize); err != nil {
		return nil, err
	}
	var mac [6]byte
	if err := binary.Read(br, binary.LittleEndian, &mac); err != nil {
		return nil, err
	}
	return &Vif{
		MTU:           MTU,
		MaxPacketSize: MaxPacketSize,
		ClientMAC:     mac[:],
	}, nil
}

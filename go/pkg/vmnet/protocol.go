package vmnet

import (
	"encoding/binary"
	"fmt"
	"io"
	"log"
	"net"

	"github.com/google/uuid"
	"github.com/pkg/errors"
)

// vpnkit internal protocol requests and responses

// InitMessage is used for the initial version exchange
type InitMessage struct {
	magic   [5]byte
	version uint32
	commit  [40]byte
}

// String returns a human-readable string.
func (m *InitMessage) String() string {
	return fmt.Sprintf("magic=%v version=%d commit=%v", m.magic, m.version, m.commit)
}

// defaultInitMessage is the init message we will send to vpnkit
func defaultInitMessage() *InitMessage {
	magic := [5]byte{'V', 'M', 'N', '3', 'T'}
	version := uint32(22)
	var commit [40]byte
	copy(commit[:], []byte("0123456789012345678901234567890123456789"))
	return &InitMessage{magic, version, commit}
}

// Write marshals an init message to a connection
func (m *InitMessage) Write(w io.Writer) error {
	if err := binary.Write(w, binary.LittleEndian, m.magic); err != nil {
		return err
	}
	if err := binary.Write(w, binary.LittleEndian, m.version); err != nil {
		return err
	}
	if err := binary.Write(w, binary.LittleEndian, m.commit); err != nil {
		return err
	}
	return nil
}

// readInitMessage unmarshals an init message from a connection
func readInitMessage(r io.Reader) (*InitMessage, error) {
	m := defaultInitMessage()
	if err := binary.Read(r, binary.LittleEndian, &m.magic); err != nil {
		return nil, err
	}
	if err := binary.Read(r, binary.LittleEndian, &m.version); err != nil {
		return nil, err
	}
	log.Printf("version = %d", m.version)
	if err := binary.Read(r, binary.LittleEndian, &m.commit); err != nil {
		return nil, err
	}
	return m, nil
}

// EthernetRequest requests the creation of a network connection with a given
// uuid and optional IP
type EthernetRequest struct {
	uuid uuid.UUID
	ip   net.IP
}

// NewEthernetRequest requests an Ethernet connection
func NewEthernetRequest(uuid uuid.UUID, ip net.IP) *EthernetRequest {
	return &EthernetRequest{uuid, ip}
}

// Write marshals an EthernetRequest message
func (m *EthernetRequest) Write(w io.Writer) error {
	ty := uint8(1)
	if m.ip != nil {
		ty = uint8(8)
	}
	if err := binary.Write(w, binary.LittleEndian, ty); err != nil {
		return err
	}
	u, err := m.uuid.MarshalText()
	if err != nil {
		return err
	}
	if err := binary.Write(w, binary.LittleEndian, u); err != nil {
		return err
	}
	ip := uint32(0)
	if m.ip != nil {
		ip = binary.BigEndian.Uint32(m.ip.To4())
	}
	// The protocol uses little endian, not network endian
	if err := binary.Write(w, binary.LittleEndian, ip); err != nil {
		return err
	}
	return nil
}

func readEthernetResponse(r io.Reader) error {
	var responseType uint8
	if err := binary.Read(r, binary.LittleEndian, &responseType); err != nil {
		return err
	}
	switch responseType {
	case 1:
		return nil
	default:
		var len uint8
		if err := binary.Read(r, binary.LittleEndian, &len); err != nil {
			return err
		}
		message := make([]byte, len)
		if err := binary.Read(r, binary.LittleEndian, &message); err != nil {
			return err
		}

		return errors.New(string(message))
	}
}

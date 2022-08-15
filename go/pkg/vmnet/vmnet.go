package vmnet

import (
	"context"
	"fmt"
	"io"
	"io/ioutil"
	"net"
	"syscall"

	"github.com/google/uuid"
	"github.com/pkg/errors"
)

// Vmnet describes a "vmnet protocol" connection which allows ethernet frames to be
// sent to and received by vpnkit.
type Vmnet struct {
	closer        io.Closer
	raw           io.ReadWriter
	packets       packetReadWriter
	remoteVersion *InitMessage
}

// New constructs an instance of Vmnet.
func New(ctx context.Context, path string) (*Vmnet, error) {
	d := &net.Dialer{}
	c, err := d.DialContext(ctx, "unix", path)
	if err != nil {
		return nil, err
	}
	remoteVersion, err := negotiate(c)
	if err != nil {
		return nil, err
	}
	vmnet := &Vmnet{c, c, ethernetStream{c}, remoteVersion}
	return vmnet, err
}

const (
	fdSendMagic   = "VMNET"
	fdSendSuccess = "OK"
)

// NewFileDescriptor returns a SOCK_DGRAM file descriptor where ethernet frames can be
// sent to vpnkit via send/recv.
func NewFileDescriptor(ctx context.Context, path string) (*Vmnet, error) {
	// Create a socketpair
	fds, err := syscall.Socketpair(syscall.AF_LOCAL, syscall.SOCK_DGRAM, 0)
	if err != nil {
		return nil, errors.Wrap(err, "creating SOCK_DGRAM socketpair for ethernet")
	}
	defer func() {
		for _, fd := range fds {
			_ = syscall.Close(fd)
		}
	}()

	for _, fd := range fds {
		maxLength := 1048576
		if err := syscall.SetsockoptInt(fd, syscall.SOL_SOCKET, syscall.SO_RCVBUF, maxLength); err != nil {
			return nil, errors.Wrap(err, "setting SO_RCVBUF")
		}
		if err := syscall.SetsockoptInt(fd, syscall.SOL_SOCKET, syscall.SO_SNDBUF, maxLength); err != nil {
			return nil, errors.Wrap(err, "setting SO_SNDBUF")
		}
	}
	// Dial over SOCK_STREAM, passing fd and magic
	c, err := net.DialUnix("unix", nil, &net.UnixAddr{Name: path, Net: "unix"})
	if err != nil {
		return nil, errors.Wrap(err, "dialing "+path)
	}
	defer c.Close()
	if err := sendFileDescriptor(c, []byte(fdSendMagic), fds[0]); err != nil {
		return nil, errors.Wrap(err, "sending file descriptor")
	}
	// Receive success
	response, err := ioutil.ReadAll(c)
	if err != nil {
		return nil, errors.Wrap(err, "reading response from file descriptor send")
	}
	if string(response) != fdSendSuccess {
		return nil, fmt.Errorf("sending file descriptor: %s", string(response))
	}
	// We can now negotiate over the socketpair
	packet := ethernetDatagram{fds[1]}
	remoteVersion, err := negotiate(packet)
	if err != nil {
		return nil, err
	}
	vmnet := &Vmnet{packet, packet, packet, remoteVersion}
	return vmnet, nil
}

func sendFileDescriptor(c *net.UnixConn, msg []byte, fd int) error {
	rights := syscall.UnixRights(fd)

	unixConnFile, err := c.File()
	if err != nil {
		return errors.Wrap(err, "can't access connection file")
	}
	defer unixConnFile.Close()

	unixConnFd := int(unixConnFile.Fd())
	return syscall.Sendmsg(unixConnFd, msg, rights, nil, 0)
}

// Close closes the connection.
func (v *Vmnet) Close() error {
	return v.closer.Close()
}

// ConnectVif returns a connected network interface with the given uuid.
func (v *Vmnet) ConnectVif(uuid uuid.UUID) (*Vif, error) {
	return connectVif(v.raw, v.packets, uuid)
}

// ConnectVifIP returns a connected network interface with the given uuid
// and IP. If the IP is already in use then return an error.
func (v *Vmnet) ConnectVifIP(uuid uuid.UUID, IP net.IP) (*Vif, error) {
	return connectVifIP(v.raw, uuid, IP)
}

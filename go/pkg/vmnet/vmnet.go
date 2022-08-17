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
	fixedSize     io.ReadWriter    // fixed-size messages used by vpnkit itself
	packets       packetReadWriter // variable-length packets
	remoteVersion *InitMessage
}

// New connection to vpnkit's ethernet socket.
func New(ctx context.Context, path string) (*Vmnet, error) {
	// use the old stream socket by default
	return connectStream(ctx, path)
}

const (
	fdSendMagic   = "VMNET"
	fdSendSuccess = "OK"
)

// ConnectDatagram connects to vpnkit using the new SOCK_DGRAM protocol.
func ConnectDatagram(ctx context.Context, path string) (*Vmnet, error) {
	// Create a socketpair
	fds, err := socketpair()
	if err != nil {
		return nil, errors.Wrap(err, "creating SOCK_DGRAM socketpair for ethernet")
	}
	defer func() {
		for _, fd := range fds {
			if fd == -1 {
				continue
			}
			_ = syscall.Close(fd)
		}
	}()

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
	datagram := Datagram{fds[1]}
	remoteVersion, err := negotiate(datagram)
	if err != nil {
		return nil, err
	}
	vmnet := &Vmnet{
		closer:        datagram,
		fixedSize:     datagram,
		packets:       datagram,
		remoteVersion: remoteVersion,
	}
	fds[1] = -1 // don't close our end of the socketpair in the defer
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

// connectStream uses the old SOCK_STREAM protocol.
func connectStream(ctx context.Context, path string) (*Vmnet, error) {
	d := &net.Dialer{}
	c, err := d.DialContext(ctx, "unix", path)
	if err != nil {
		return nil, err
	}
	remoteVersion, err := negotiate(c)
	if err != nil {
		return nil, err
	}
	vmnet := &Vmnet{
		closer:        c,
		fixedSize:     c,
		packets:       ethernetFramer{c}, // need to add artificial message boundaries
		remoteVersion: remoteVersion,
	}
	return vmnet, err
}

func (v *Vmnet) Close() error {
	return v.closer.Close()
}

// ConnectVif returns a connected network interface with the given uuid.
func (v *Vmnet) ConnectVif(uuid uuid.UUID) (*Vif, error) {
	return connectVif(v.fixedSize, v.packets, uuid)
}

// ConnectVifIP returns a connected network interface with the given uuid
// and IP. If the IP is already in use then return an error.
func (v *Vmnet) ConnectVifIP(uuid uuid.UUID, IP net.IP) (*Vif, error) {
	return connectVifIP(v.fixedSize, v.packets, uuid, IP)
}

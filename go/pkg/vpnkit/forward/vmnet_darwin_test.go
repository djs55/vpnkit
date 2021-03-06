package forward

import (
	"bytes"
	"fmt"
	"io"
	"net"
	"syscall"

	"github.com/moby/vpnkit/go/pkg/vpnkit"
	"github.com/stretchr/testify/assert"

	"testing"
	"time"
)

func TestMarshalInit(t *testing.T) {
	h := getOutgoingMessage()
	var buf bytes.Buffer
	assert.Nil(t, writeInitMessage(&buf, h))
	hh, err := readInitMessage(&buf)
	assert.Nil(t, err)
	assert.Equal(t, h, hh)

}

func TestMarshalCommand(t *testing.T) {
	c := bindIpv4Command
	var buf bytes.Buffer
	assert.Nil(t, writeCommand(&buf, c))
	cc, err := readCommand(&buf)
	assert.Nil(t, err)
	assert.Equal(t, c, cc)
}

func TestMarshalBindIpv4(t *testing.T) {
	b := bindIpv4{
		IP:   net.ParseIP("127.0.0.1"),
		Port: 1234,
		TCP:  false,
	}
	var buf bytes.Buffer
	assert.Nil(t, writeBindIpv4(&buf, b))
	bb, err := readBindIpv4(&buf)
	assert.Nil(t, err)
	assert.Equal(t, b, *bb)
}

func TestBindVmnetdLow(t *testing.T) {
	for i := 0; i < 10; i++ {
		fd, err := listenVmnet(localhost, 8081, true)
		assert.Nil(t, err)
		assert.Nil(t, syscall.Close(int(fd)))
	}
}

func TestBindUDPVmnetd(t *testing.T) {
	localhost := net.ParseIP("127.0.0.1")
	addr := &net.UDPAddr{
		IP:   localhost,
		Port: 8081,
	}
	all := net.ParseIP("0.0.0.0")
	f, err := listenUDPVmnet(all, uint16(addr.Port))
	assert.Nil(t, err)
	defer f.Close()
	hello := []byte("hello\n")
	done := make(chan struct{})

	go func() {
		client, err := net.DialUDP("udp", nil, addr)
		assert.Nil(t, err)
		assert.NotNil(t, client)
		defer client.Close()
		for i := 0; i < 100; i++ {
			n, err := client.Write(hello)
			assert.Nil(t, err)
			select {
			case <-done:
				return
			default:
				fmt.Printf("sent %d bytes, sleeping for 0.1s\n", n)
				time.Sleep(100 * time.Millisecond)
			}
		}
		t.FailNow()
	}()
	result := make([]byte, 1024)
	n, err := f.Read(result)
	assert.Nil(t, err)
	assert.Equal(t, string(hello), string(result[0:n]))
	done <- struct{}{}
}

func TestBindTCPVmnetd(t *testing.T) {
	localhost := net.ParseIP("127.0.0.1")
	f, err := listenTCPVmnet(localhost, 8081)
	assert.Nil(t, err)
	hello := []byte("hello\n")
	done := make(chan struct{})
	go func() {
		client, err := net.Dial("tcp", "localhost:8081")
		assert.Nil(t, err)
		assert.NotNil(t, client)
		buf := bytes.NewBuffer(hello)
		_, err = io.Copy(client, buf)
		assert.Nil(t, err)
		assert.Nil(t, client.Close())
		done <- struct{}{}
	}()
	conn, err := f.Accept()
	assert.Nil(t, err)
	var buf bytes.Buffer
	_, err = io.Copy(&buf, conn)
	assert.Nil(t, err)
	assert.Equal(t, hello, buf.Bytes())
	assert.Nil(t, conn.Close())
	assert.Nil(t, f.Close())
	time.Sleep(time.Second)
	<-done
}

func TestBindTCPVmnetdLeak(t *testing.T) {
	for i := 0; i < 10; i++ {
		TestBindTCPVmnetd(t)
	}
}

func TestBindUDPVmnetdLeak(t *testing.T) {
	for i := 0; i < 10; i++ {
		TestBindUDPVmnetd(t)
	}
}

func TestBindTCPVmnetdClose(t *testing.T) {
	localhost := net.ParseIP("127.0.0.1")
	f, err := listenTCPVmnet(localhost, 8081)
	assert.Nil(t, err)
	go func() {
		c, err := f.Accept()
		if c != nil {
			c.Close()
		}
		assert.Nil(t, err)
	}()
	time.Sleep(10 * time.Millisecond)
	assert.Nil(t, closeTCPVmnet(localhost, 8081, f))
}

func TestBindUDPVmnetdClose(t *testing.T) {
	localhost := net.ParseIP("127.0.0.1")
	f, err := listenUDPVmnet(localhost, 8081)
	assert.Nil(t, err)
	go func() {
		buf := make([]byte, 1024)
		_, err := f.Read(buf)
		assert.Nil(t, err)
	}()
	time.Sleep(10 * time.Millisecond)
	assert.Nil(t, closeUDPVmnet(localhost, 8081, f))
}

func TestBindTCPVmnetdCloseLeak(t *testing.T) {
	for i := 0; i < 10; i++ {
		TestBindTCPVmnetdClose(t)
	}
}

func TestBindUDPVmnetdCloseLeak(t *testing.T) {
	for i := 0; i < 10; i++ {
		TestBindUDPVmnetdClose(t)
	}
}

func TestListenUDPMojave1(t *testing.T) {
	// On Mojave this will not need vmnetd
	l, err := listenUDP(vpnkit.Port{
		OutIP:   net.ParseIP("0.0.0.0"),
		OutPort: 80,
	})
	assert.Nil(t, err)
	assert.Nil(t, l.Close())
}

func TestListenUDPMojave2(t *testing.T) {
	// On Mojave this will need vmnetd
	l, err := listenUDP(vpnkit.Port{
		OutIP:   net.ParseIP("127.0.0.1"),
		OutPort: 80,
	})
	assert.Nil(t, err)
	assert.Nil(t, l.Close())
}

func TestListenTCPMojave1(t *testing.T) {
	// On Mojave this will not need vmnetd
	l, vmnetd, err := listenTCP(vpnkit.Port{
		OutIP:   net.ParseIP("0.0.0.0"),
		OutPort: 80,
	})
	assert.Nil(t, err)
	assert.Equal(t, false, vmnetd)
	assert.Nil(t, l.Close())
}

func TestListenTCPMojave2(t *testing.T) {
	// On Mojave this will need vmnetd
	l, vmnetd, err := listenTCP(vpnkit.Port{
		OutIP:   net.ParseIP("127.0.0.1"),
		OutPort: 80,
	})
	assert.Nil(t, err)
	assert.Equal(t, true, vmnetd)
	assert.Nil(t, l.Close())
}

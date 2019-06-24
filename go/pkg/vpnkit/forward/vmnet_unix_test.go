//+build !windows

package forward

import (
	"bytes"
	"github.com/stretchr/testify/assert"
	"io"
	"net"
	"syscall"

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
	for i := 0; i < 10; i ++ {
		fd, err := listenVmnet(localhost, 8081, true)
		assert.Nil(t, err)
		assert.Nil(t, syscall.Close(int(fd)))
	}
}

func TestBindVmnetd(t *testing.T) {
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
	<- done
}

func TestBindVmnetdLeak(t *testing.T){
	for i :=0; i < 10; i ++{
		TestBindVmnetd(t)
	}
}

func TestBindVmnetdClose(t *testing.T) {
	localhost := net.ParseIP("127.0.0.1")
	f, err := listenTCPVmnet(localhost, 8081)
	assert.Nil(t, err)
	go func(){
		c, err := f.Accept()
		if c != nil {
			c.Close()
		}
		assert.Nil(t, err)
	} ()
	time.Sleep(10 * time.Millisecond)
	assert.Nil(t, closeTCPVmnet(localhost, 8081, f))
}

func TestBindVmnetdCloseLeak(t *testing.T){
	for i := 0; i < 10; i ++{
		TestBindVmnetdClose(t)
	}
}
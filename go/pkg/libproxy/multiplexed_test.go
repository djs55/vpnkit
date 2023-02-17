package libproxy

import (
	"bytes"
	"crypto/rand"
	"crypto/sha1"
	"encoding/binary"
	"fmt"
	"io"
	"net"
	"sync"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"golang.org/x/sync/errgroup"
)

const simulatedNetworkBufferSize = 1024

func newLoopback(t *testing.T) *loopback {
	l := NewLoopback()
	require.Nil(t, l.SetWriteBuffer(simulatedNetworkBufferSize))
	require.Nil(t, l.OtherEnd().SetWriteBuffer(simulatedNetworkBufferSize))
	return l
}

// create a pair of connected multiplexers
func newLoopbackMultiplexer(t *testing.T, loopback *loopback) (Multiplexer, Multiplexer) {
	localMuxC, localErrC := newMultiplexer("local", loopback, false)
	remoteMuxC, remoteErrC := newMultiplexer("remote", loopback.OtherEnd(), true)
	require.Nil(t, <-localErrC)
	require.Nil(t, <-remoteErrC)
	local := <-localMuxC
	remote := <-remoteMuxC
	local.Run()
	remote.Run()
	return local, remote
}

// start a multiplexer asynchronously
func newMultiplexer(name string, conn io.ReadWriteCloser, allocateBackwards bool) (<-chan Multiplexer, <-chan error) {
	m := make(chan Multiplexer)
	e := make(chan error)
	go func() {
		mux, err := NewMultiplexer(name, conn, allocateBackwards)
		e <- err
		m <- mux
	}()
	return m, e
}

func TestNew(t *testing.T) {
	loopback := newLoopback(t)
	local, remote := newLoopbackMultiplexer(t, loopback)
	client, err := local.Dial(Destination{
		Proto: TCP,
		IP:    net.ParseIP("127.0.0.1"),
		Port:  8080,
	})
	require.Nil(t, err)
	server, _, err := remote.Accept()
	require.Nil(t, err)
	assert.Nil(t, client.Close())
	assert.Nil(t, server.Close())
	assert.Nil(t, local.Close())
	assert.Nil(t, remote.Close())
}

func TestClose(t *testing.T) {
	loopback := newLoopback(t)
	local, remote := newLoopbackMultiplexer(t, loopback)
	// There was a bug where the second iteration failed because the main loop had deadlocked
	// when it received a Close message.
	for i := 0; i < 2; i++ {
		client, err := local.Dial(Destination{
			Proto: TCP,
			IP:    net.ParseIP("127.0.0.1"),
			Port:  8080,
		})
		require.Nil(t, err)
		server, _, err := remote.Accept()
		require.Nil(t, err)
		require.Nil(t, client.Close())
		require.Nil(t, server.Close())
	}
}

func TestUDPEncapsulationIsTransparent(t *testing.T) {
	loopback := newLoopback(t)
	local, remote := newLoopbackMultiplexer(t, loopback)

	client, err := local.Dial(Destination{
		Proto: UDP,
		IP:    net.ParseIP("127.0.0.1"),
		Port:  8080,
	})
	require.Nil(t, err)
	server, _, err := remote.Accept()
	require.Nil(t, err)
	message := []byte("hello world")
	n, err := client.Write(message)
	require.Nil(t, err)
	assert.Equal(t, len(message), n)
	buf := make([]byte, 1024)
	n, err = server.Read(buf)
	require.Nil(t, err)
	assert.Equal(t, len(message), n)
	require.Nil(t, client.Close())
	require.Nil(t, server.Close())
}

func TestCloseClose(t *testing.T) {
	loopback := newLoopback(t)
	local, remote := newLoopbackMultiplexer(t, loopback)
	// There was a bug where the second iteration failed because the main loop had deadlocked
	// when it received a Close message.
	for i := 0; i < 2; i++ {
		client, err := local.Dial(Destination{
			Proto: TCP,
			IP:    net.ParseIP("127.0.0.1"),
			Port:  8080,
		})
		require.Nil(t, err)
		server, _, err := remote.Accept()
		require.Nil(t, err)
		require.Nil(t, client.Close())
		require.Nil(t, client.Close())
		require.Nil(t, server.Close())
		require.True(t, remote.IsRunning())
	}
}

func TestCloseWriteCloseWrite(t *testing.T) {
	loopback := newLoopback(t)
	local, remote := newLoopbackMultiplexer(t, loopback)
	// There was a bug where the second iteration failed because the main loop had deadlocked
	// when it received a Close message.
	for i := 0; i < 2; i++ {
		client, err := local.Dial(Destination{
			Proto: TCP,
			IP:    net.ParseIP("127.0.0.1"),
			Port:  8080,
		})
		require.Nil(t, err)
		server, _, err := remote.Accept()
		require.Nil(t, err)
		require.Nil(t, client.CloseWrite())
		require.Nil(t, client.CloseWrite())
		require.Nil(t, server.Close())
		require.True(t, remote.IsRunning())
	}
}

func TestCloseCloseWrite(t *testing.T) {
	loopback := newLoopback(t)
	local, remote := newLoopbackMultiplexer(t, loopback)
	// There was a bug where the second iteration failed because the main loop had deadlocked
	// when it received a Close message.
	for i := 0; i < 2; i++ {
		client, err := local.Dial(Destination{
			Proto: TCP,
			IP:    net.ParseIP("127.0.0.1"),
			Port:  8080,
		})
		require.Nil(t, err)
		server, _, err := remote.Accept()
		require.Nil(t, err)
		require.Nil(t, client.Close())
		require.Nil(t, client.CloseWrite())
		require.Nil(t, server.Close())
		require.True(t, remote.IsRunning())
	}
}

func TestCloseWriteWrite(t *testing.T) {
	loopback := newLoopback(t)
	local, remote := newLoopbackMultiplexer(t, loopback)
	client, err := local.Dial(Destination{
		Proto: TCP,
		IP:    net.ParseIP("127.0.0.1"),
		Port:  8080,
	})
	require.Nil(t, err)
	server, _, err := remote.Accept()
	require.Nil(t, err)
	channel, ok := client.(*channel)
	require.True(t, ok)
	channel.setTestAllowDataAfterCloseWrite()
	require.Nil(t, client.CloseWrite())
	_, err = client.Write([]byte{1})
	require.Nil(t, err)
	// FIXME: need a way to wait for the multiplexer to have processed the message.
	time.Sleep(time.Second)
	require.True(t, remote.IsRunning())
	require.Nil(t, server.Close())
}

func TestCloseThenWrite(t *testing.T) {
	loopback := newLoopback(t)
	local, remote := newLoopbackMultiplexer(t, loopback)
	// There was a bug where the second iteration failed because the main loop had deadlocked
	// when it received a Close message.
	for i := 0; i < 2; i++ {
		client, err := local.Dial(Destination{
			Proto: TCP,
			IP:    net.ParseIP("127.0.0.1"),
			Port:  8080,
		})
		require.Nil(t, err)
		server, _, err := remote.Accept()
		require.Nil(t, err)
		loopback.simulateLatency = time.Second
		done := make(chan struct{})
		go func() {
			for {
				if _, err := client.Write([]byte{1}); err != nil {
					close(done)
					return // EOF
				}
			}
		}()
		require.Nil(t, client.Close())
		<-done
		require.True(t, remote.IsRunning())
		require.Nil(t, server.Close())
	}
}

func TestReadDeadline(t *testing.T) {
	loopback := newLoopback(t)
	local, remote := newLoopbackMultiplexer(t, loopback)
	client, err := local.Dial(Destination{
		Proto: TCP,
		IP:    net.ParseIP("127.0.0.1"),
		Port:  8080,
	})
	require.Nil(t, err)
	server, _, err := remote.Accept()
	require.Nil(t, err)
	// Server never writes
	require.Nil(t, client.SetReadDeadline(time.Now().Add(time.Second)))
	var b []byte
	_, err = client.Read(b)
	require.Error(t, &errTimeout{}, err)
	require.Nil(t, client.Close())
	require.Nil(t, server.Close())
}

func TestWriteDeadline(t *testing.T) {
	loopback := newLoopback(t)
	local, remote := newLoopbackMultiplexer(t, loopback)
	client, err := local.Dial(Destination{
		Proto: TCP,
		IP:    net.ParseIP("127.0.0.1"),
		Port:  8080,
	})
	require.Nil(t, err)
	server, _, err := remote.Accept()
	require.Nil(t, err)
	require.Nil(t, server.SetWriteDeadline(time.Now().Add(time.Second)))
	done := make(chan error)
	go func() {
		buf, _ := genRandomBuffer(1024)
		for {
			if _, err := server.Write(buf); err != nil {
				done <- err
			}
		}
	}()
	// Client never reads so the window should close
	<-done
	assert.Nil(t, client.Close())
	assert.Nil(t, server.Close())
}

func genRandomBuffer(size int) ([]byte, string) {
	buf := make([]byte, size)
	_, _ = rand.Read(buf)
	return buf, fmt.Sprintf("% x", sha1.Sum(buf))
}

func writeRandomBuffer(w Conn, toWriteClient int) (chan error, string) {
	clientWriteBuf, clientWriteSha := genRandomBuffer(toWriteClient)
	done := make(chan error, 1)

	go func() {
		if _, err := w.Write(clientWriteBuf); err != nil {
			done <- err
		}
		done <- w.CloseWrite()
	}()
	return done, clientWriteSha
}

func readAndSha(t *testing.T, r Conn) chan string {
	result := make(chan string)
	go func() {
		var toRead bytes.Buffer
		_, err := io.Copy(&toRead, r)
		if err != nil {
			result <- fmt.Sprintf("error: %s", err)
			return
		}
		sha := fmt.Sprintf("% x", sha1.Sum(toRead.Bytes()))
		result <- sha
	}()
	return result
}

func muxReadWrite(t *testing.T, local, remote Multiplexer, toWriteClient, toWriteServer int) {
	client, err := local.Dial(Destination{
		Proto: TCP,
		IP:    net.ParseIP("127.0.0.1"),
		Port:  8080,
	})
	require.Nil(t, err)
	clientWriteErr, clientWriteSha := writeRandomBuffer(client, toWriteClient)

	server, _, err := remote.Accept()
	require.Nil(t, err)

	serverWriteErr, serverWriteSha := writeRandomBuffer(server, toWriteServer)

	serverReadShaC := readAndSha(t, server)
	clientReadShaC := readAndSha(t, client)
	serverReadSha := <-serverReadShaC
	clientReadSha := <-clientReadShaC
	assertEqual(t, clientWriteSha, serverReadSha)
	assertEqual(t, serverWriteSha, clientReadSha)

	require.Nil(t, <-clientWriteErr)
	require.Nil(t, <-serverWriteErr)
	require.Nil(t, client.Close())
	require.Nil(t, server.Close())
}

var (
	interesting = []int{
		0,
		1,
		4,
		4095,
		4096,
		4097,
		4098,
		4099,
		5000,
		5001,
		5002,
		1048575,
		1048576,
		1048577,
	}
)

func TestMuxCorners(t *testing.T) {
	for _, toWriteClient := range interesting {
		for _, toWriteServer := range interesting {
			log.Printf("Client will write %d and server will write %d", toWriteClient, toWriteServer)
			loopback := newLoopback(t)
			local, remote := newLoopbackMultiplexer(t, loopback)
			muxReadWrite(t, local, remote, toWriteClient, toWriteServer)
		}
	}
}

func TestMuxReadWrite(t *testing.T) {
	loopback := newLoopback(t)
	local, remote := newLoopbackMultiplexer(t, loopback)
	muxReadWrite(t, local, remote, 1048576, 1048576)
}

func TestMuxConcurrent(t *testing.T) {
	loopback := newLoopback(t)
	local, remote := newLoopbackMultiplexer(t, loopback)
	numConcurrent := 500 // limited by the race detector
	toWrite := 65536 * 2 // 2 * Window size
	serverWriteSha := make(map[uint16]string)
	serverReadSha := make(map[uint16]string)
	clientWriteSha := make(map[uint16]string)
	clientReadSha := make(map[uint16]string)
	var g errgroup.Group
	m := &sync.Mutex{}
	for i := 0; i < numConcurrent; i++ {
		g.Go(func() error {
			server, destination, err := remote.Accept()
			if err != nil {
				return err
			}
			defer server.Close()
			// Set the read/write buffer sizes to unusual values.
			server.SetReadBuffer(defaultWindowSize - 1)
			server.SetWriteBuffer(defaultWindowSize - 1)
			done, sha := writeRandomBuffer(server, toWrite)
			m.Lock()
			serverWriteSha[destination.Port] = sha
			m.Unlock()

			shaC := readAndSha(t, server)
			sha = <-shaC
			m.Lock()
			serverReadSha[destination.Port] = sha
			m.Unlock()

			return <-done
		})
	}

	for i := uint16(0); i < uint16(numConcurrent); i++ {
		i := i
		g.Go(func() error {
			client, err := local.Dial(Destination{
				Proto: TCP,
				IP:    net.ParseIP("127.0.0.1"),
				Port:  i,
			})
			if err != nil {
				return err
			}
			defer client.Close()
			done, sha := writeRandomBuffer(client, toWrite)
			m.Lock()
			clientWriteSha[i] = sha
			m.Unlock()

			shaC := readAndSha(t, client)
			sha = <-shaC
			m.Lock()
			clientReadSha[i] = sha
			m.Unlock()
			return <-done
		})
	}
	require.Nil(t, g.Wait())
	failed := false
	for i := uint16(0); i < uint16(numConcurrent); i++ {
		if clientWriteSha[i] != serverReadSha[i] {
			fmt.Printf("clientWriteSha[%d] = %s\nserverReadSha[%d] = %s\n", i, clientWriteSha[i], i, serverReadSha[i])
			failed = true
		}
		if serverWriteSha[i] != clientReadSha[i] {
			fmt.Printf("serverWriteSha[%d] = %s\nclientReadSha[%d] = %s\n", i, serverWriteSha[i], i, clientReadSha[i])
			failed = true
		}
	}
	require.False(t, failed)
}

func writeAndBlock(t *testing.T, local, remote Multiplexer) chan error {
	client, err := local.Dial(Destination{
		Proto: TCP,
		IP:    net.ParseIP("127.0.0.1"),
		Port:  8080,
	})
	if err != nil {
		t.Fatal(err)
	}
	server, _, err := remote.Accept()
	require.Nil(t, err)
	require.Nil(t, server.SetWriteDeadline(time.Now().Add(1*time.Second)))
	done := make(chan error, 2)
	go func() {
		buf, _ := genRandomBuffer(1024)
		for {
			// Client never reads so the window should close
			if _, err := server.Write(buf); err != nil {
				fmt.Printf("server.Write failed with %v", err)
				break
			}
		}
		if err := client.Close(); err != nil {
			done <- err
		}
		if err := server.Close(); err != nil {
			done <- err
		}
		close(done)
	}()
	// (hack) wait until the window must be full and the Write is blocked
	time.Sleep(500 * time.Millisecond)
	return done
}

func TestWindow(t *testing.T) {
	// Check that one connection blocked on a window update doesn't preclude
	// other connections from working i.e. the lowlevel connection handler isn't
	// itself blocked in a write()
	loopback := newLoopback(t)
	local, remote := newLoopbackMultiplexer(t, loopback)

	done := writeAndBlock(t, local, remote)
	// The first connection should have blocked and the window should be closed for another 500ms
	muxReadWrite(t, local, remote, 1048576, 1048576)

	select {
	case err := <-done:
		require.Nil(t, err)
		t.Fatalf("the second connection was blocked by the first")
	default:
		fmt.Println("second connection completed while the first was blocked")
		err := <-done
		require.Nil(t, err)
		fmt.Println("first connection has now unblocked")
	}
}

func TestEOF(t *testing.T) {
	loopback := newLoopback(t)
	require.Nil(t, loopback.OtherEnd().Close())
	_, err := NewMultiplexer("test", loopback, false)
	assert.Equal(t, io.EOF, err)
}

func TestCrossChannelOpening(t *testing.T) {
	loopback := newLoopback(t)
	muxHost, muxGuest := newLoopbackMultiplexer(t, loopback)
	acceptG := &errgroup.Group{}
	acceptG.Go(func() error {
		for {
			c, _, err := muxHost.Accept()
			if err != nil {
				return err
			}
			var m int32
			if err := binary.Read(c, binary.LittleEndian, &m); err != nil {
				return err
			}
			if err := binary.Write(c, binary.LittleEndian, m); err != nil {
				return err
			}
			if err := c.Close(); err != nil {
				return err
			}
		}
	})
	acceptG.Go(func() error {
		for {
			c, _, err := muxGuest.Accept()
			if err != nil {
				return err
			}
			var m int32
			if err := binary.Read(c, binary.LittleEndian, &m); err != nil {
				return err
			}
			if err := binary.Write(c, binary.LittleEndian, m); err != nil {
				return err
			}
			if err := c.Close(); err != nil {
				return err
			}
		}
	})
	g := &errgroup.Group{}
	for i := 0; i < 20; i++ {
		g.Go(func() error {
			c, err := muxHost.Dial(Destination{Proto: Unix, Path: "/test"})
			if err != nil {
				return err
			}
			m := int32(42)

			if err := binary.Write(c, binary.LittleEndian, m); err != nil {
				return err
			}
			if err := binary.Read(c, binary.LittleEndian, &m); err != nil {
				return err
			}
			if err := c.Close(); err != nil {
				return err
			}
			log.Print("muxHost negociation succeeded")
			return nil
		})
		g.Go(func() error {
			c, err := muxGuest.Dial(Destination{Proto: Unix, Path: "/test"})
			if err != nil {
				return err
			}
			m := int32(42)

			if err := binary.Write(c, binary.LittleEndian, m); err != nil {
				return err
			}
			if err := binary.Read(c, binary.LittleEndian, &m); err != nil {
				return err
			}
			if err := c.Close(); err != nil {
				return err
			}
			log.Print("muxGuest negociation succeeded")
			return nil
		})
	}
	assert.Nil(t, g.Wait())
}

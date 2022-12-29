package main

import (
	"context"
	"flag"
	"io"
	"log"
	"net"
	"os"
	"syscall"
	"time"

	"github.com/moby/vpnkit/go/pkg/libproxy"
	"golang.org/x/sync/errgroup"
)

func main() {
	len := flag.Int("len", 1024*1024*1024, "bytes to send")
	useSocketPair := flag.Bool("socketpair", false, "use a real OS socketpair")
	latencyMs := flag.Int("latency", 0, "latency in ms to simulate")
	flag.Parse()
	over := "in memory connection"
	var a, b net.Conn
	var err error
	if *useSocketPair {
		a, b, err = socketpair()
		over = "syscall.Socketpair"
	} else {
		a, b, err = loopbackpair(*latencyMs)
	}
	if err != nil {
		log.Fatal(err)
	}
	log.Printf("Will send %d bytes over %s", *len, over)
	mA, mB, err := newConnectedMultiplexers(a, b)
	if err != nil {
		log.Fatal(err)
	}
	log.Println("Starting test")
	start := time.Now()
	errGroup, _ := errgroup.WithContext(context.Background())
	errGroup.Go(func() error {
		conn, _, err := mA.Accept()
		if err != nil {
			return err
		}
		defer conn.Close()

		_, err = io.Copy(conn, io.LimitReader(&reader{}, int64(*len)))
		return err
	})
	errGroup.Go(func() error {
		conn, err := mB.Dial(libproxy.Destination{})
		if err != nil {
			return err
		}
		defer conn.Close()

		_, err = io.Copy(io.Discard, conn)
		return err
	})
	if err := errGroup.Wait(); err != nil {
		log.Fatal(err)
	}
	bytesPerSec := (uint64(*len) * 1e9) / uint64(time.Since(start).Nanoseconds())
	log.Printf("Copied %d in %s: %d GiB per second", *len, time.Since(start), bytesPerSec/(1024*1024))
}

func loopbackpair(latencyMs int) (net.Conn, net.Conn, error) {
	l := libproxy.NewLoopback()
	l.SimulateLatency(time.Millisecond * time.Duration(latencyMs))
	l2 := l.OtherEnd()
	l2.SimulateLatency(time.Millisecond * time.Duration(latencyMs))
	return l, l2, nil
}

func newConnectedMultiplexers(a, b net.Conn) (libproxy.Multiplexer, libproxy.Multiplexer, error) {
	localMuxC, localErrC := newMultiplexer("local", a, false)
	remoteMuxC, remoteErrC := newMultiplexer("remote", b, true)
	if err := <-localErrC; err != nil {
		return nil, nil, err
	}
	if err := <-remoteErrC; err != nil {
		return nil, nil, err

	}
	local := <-localMuxC
	remote := <-remoteMuxC
	local.Run()
	remote.Run()
	return local, remote, nil
}

func newMultiplexer(name string, conn io.ReadWriteCloser, allocateBackwards bool) (<-chan libproxy.Multiplexer, <-chan error) {
	m := make(chan libproxy.Multiplexer)
	e := make(chan error)
	go func() {
		mux, err := libproxy.NewMultiplexer(name, conn, allocateBackwards)
		e <- err
		m <- mux
	}()
	return m, e
}

func socketpair() (net.Conn, net.Conn, error) {
	fds, err := syscall.Socketpair(syscall.AF_LOCAL, syscall.SOCK_STREAM, 0)
	if err != nil {
		return nil, nil, err
	}

	c1, err := toConn(fds[0])
	if err != nil {
		return nil, nil, err
	}

	c2, err := toConn(fds[1])
	if err != nil {
		_ = c1.Close()
		return nil, nil, err
	}

	return c1, c2, err
}

func toConn(fd int) (net.Conn, error) {
	f := os.NewFile(uintptr(fd), "socketpair")
	defer f.Close()
	return net.FileConn(f)
}

type reader struct{}

func (f *reader) Read(b []byte) (int, error) {
	toRead := len(b)
	for i := 0; i < toRead; i++ {
		b[i] = byte(i)
	}
	return toRead, nil
}

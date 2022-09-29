let max_ip_datagram_length = 65535

(* IP datagram (65535) - IP header(20) - UDP header(8) *)
let max_udp_length = max_ip_datagram_length - 20 - 8

(* MTUs higher than this value break the TCP/IP stack *)
let max_working_mtu = 16424
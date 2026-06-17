FROM ocaml/opam:alpine-ocaml-5.3 AS build
RUN opam update

ADD . /home/opam/vpnkit
RUN opam pin add vpnkit /home/opam/vpnkit --kind=path -n
RUN opam install vpnkit -y

FROM scratch AS binary
COPY --from=build /home/opam/.opam/5.3/bin/vpnkit /vpnkit

FROM alpine:latest
COPY --from=binary /vpnkit /vpnkit

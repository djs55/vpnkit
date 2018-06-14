#!/usr/bin/env sh

# Create a fresh clone of opam-repository, overlay some extra packages,
# run the solver, download the sources of packages which build with jbuilder
# and list the opam metadata of packages which don't.
#
# If the non-jbuilder packages are installed with `opam`, the resulting
# workspace should build with `make`.
#
# Remaining issue:
#
# If a non-jbuilder package (like cmdliner) depends on a jbuilder package
# (like result) then it's not possible to vendor the dependency otherwise
# there is a clash.

set -ex

OPAM_SWITCH=${OPAM_SWITCH:-4.06.0}
OPAM_REPO=${OPAM_REPO:-https://github.com/ocaml/opam-repository.git}
OPAM_OS=${OPAM_OS:-darwin}

#########

REPO_DIR_NAME=opam-snapshot

TARGET_DIR=$(pwd)/${OPAM_OS}
VENDOR=$(pwd)/vendor-package.sh
VENDOR_DIR=$(pwd)/../vendor
WORK_DIR=$(mktemp -d 2>/dev/null || mktemp -d -t 'opam-mini-repo')

cleanup() {
    rm -rf "${WORK_DIR}"
}

trap cleanup EXIT

## Fetch the opam-repository

cd "${WORK_DIR}"
git clone --depth=1 ${OPAM_REPO} ${REPO_DIR_NAME}
cd ${REPO_DIR_NAME}

## copy the dev/ and local/ packages in the repo
[ -d  "${TARGET_DIR}/packages/dev" ] && \
    cp -LR "${TARGET_DIR}/packages/dev" packages/dev
[ -d "${TARGET_DIR}/packages/dev" ] && \
    git add packages/dev

cp -LR "${TARGET_DIR}/packages/local" packages/local
git add packages/local

git commit -a -m "Adding local and dev packages"

# Remove the upstream packages that are copied in /dev
if [ -d  packages/dev ]; then
  for pkg in $(ls packages/dev); do
    upstream="packages/${pkg%%.*}/${pkg}"
    if [ -d "${upstream}" ]; then
      rm -rf "${upstream}"
    fi
  done;
  git commit -a -m "Remove upstream source of dev packages" || echo "ok"
fi

## Compute the list of packages needed

PACKAGES="$* $(ls packages/local | xargs) $(ls packages/dev | xargs)"
echo "PACKAGES=${PACKAGES}"

export OPAMROOT=${WORK_DIR}/opam

opam init --root=${OPAMROOT} -n .

export OPAMSWITCH=${OPAM_SWITCH}
export OPAMNO=1

# ugly hack to make opam think that the switch is already installed
# and to overwrite opam internal variables
echo "${OPAM_SWITCH} ${OPAM_SWITCH}" > ${OPAMROOT}/aliases
mkdir -p "${OPAMROOT}/${OPAM_SWITCH}/config"

config="${OPAMROOT}/${OPAM_SWITCH}/config/global-config.config"

function add {
    key=$1
    value=$2
    echo "${key}: \"${value}\"" > ${config}
}

add ocaml_version ${OPAM_SWITCH%%+*}
add compiler ${OPAM_SWITCH}
add preinstalled false
add os ${OPAM_OS}

echo "ocaml-version=$(opam config var ocaml-version)"
echo "compiler=$(opam config var compiler)"
echo "preinstalled=$(opam config var preinstalled)"
echo "os=$(opam config var os)"

OUTPUT=${WORK_DIR}/pkgs.json
opam install --root=${OPAMROOT} ${PACKAGES} --dry-run --json=${OUTPUT}

ALL_PACKAGES=$(jq '.[] | map(select(.install)) | map( [.install.name, .install.version] | join(".")) | join(" ")' ${OUTPUT})

function vendor_package(){
  local pkg_dir=$1
  local pkg=$(basename "${pkg_dir}")
  v_dir="${pkg}"
  local pkg_no_version=$(echo "${pkg}" | cut -f 1 -d ".")
  if [ -n "$(find ${VENDOR_DIR} -name ${pkg_no_version}.opam)" ]; then
    # This happens if two packages share the same source.
    # Note that the two source archives might have different versions, for
    # example we might try to install mirage-clock-unix.1.3.0 with mirage-clock.1.2.0
    # In this case we take the first version.
    echo "${pkg} has already been vendored"
  else
    echo "${pkg} will be vendored to ${v_dir}"
    mkdir -p $(dirname "${VENDOR_DIR}/${v_dir}")
    opam source --root="${OPAMROOT}" --dir="${VENDOR_DIR}/${v_dir}" "${pkg}"
  fi
}

rm -rf "${TARGET_DIR}/packages/install-from-opam/"
mkdir -p "${TARGET_DIR}/packages/install-from-opam/"
rm -rf "${VENDOR_DIR}"
mkdir -p "${VENDOR_DIR}"
for pkg in ${ALL_PACKAGES//\"}; do
  echo "${pkg}"

  # The local packages are not vendored: the code is already here.
  # We attempt to vendor other packages; if we fail we copy the metadata into the
  # directory install-from-opam/

  if [ -d ${TARGET_DIR}/packages/dev/${pkg} ]; then
     opam_dir="${TARGET_DIR}/packages/dev/${pkg}"
     if ! cat "${opam_dir}/opam" | grep jbuilder > /dev/null 2> /dev/null; then
       echo "${pkg} does not depend on jbuilder -- cannot vendor"
       cp -R "${opam_dir}" "${TARGET_DIR}/packages/install-from-opam/"
     else
       vendor_package "${opam_dir}"
     fi
  elif [ -d ${TARGET_DIR}/packages/local/${pkg} ]; then
      echo "${pkg} is a local package, skipping."
  else
     if ! opam show --root="${OPAMROOT}" "${pkg}" | grep jbuilder > /dev/null 2> /dev/null; then
       echo "${pkg} does not depend on jbuilder -- cannot vendor"
       cp -R packages/${pkg%%.*}/${pkg} "${TARGET_DIR}/packages/install-from-opam/"
     else
       opam_dir=$(dirname $(opam show --root="${OPAMROOT}" "${pkg}" --where))
       vendor_package "${opam_dir}"
     fi
  fi
done


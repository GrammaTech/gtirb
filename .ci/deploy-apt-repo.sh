#!/bin/bash
#
# deploy-apt-repo.sh
#
# Usage:
#   ./deploy_directory.sh deploy_directory
#
# Summary:
#   Creates two apt repositories underneath deploy_directory (one for xenial,
#   one for bionic).  The created directory structure is:
#     - deploy_directory
#       - xenial
#         - [ubuntu16 packages]
#         - Packages.gz
#       - bionic
#         - [ubuntu18 packages]
#         - Packages.gz
#
#   deploy_directory gets completely whiped out and recreated by this script,
#   so be careful.  For the moment, the only package we include is gtirb.  We
#   pull the package from the master branch's job artifacts.
#
#   In order to install packages from the created repositories, one would add
#   one of the above directories to their /etc/apt/sources.list file (see the
#   'Build' section of README.md).

set -o xtrace
set -o nounset
set -o errexit
set -o pipefail

DEPLOY_DIR=$1

rm -rf $DEPLOY_DIR
mkdir -p $DEPLOY_DIR

function setup_repo {
  local OS_NUM=$1
  local CODENAME=$2
  mkdir $DEPLOY_DIR/$CODENAME
  pushd $DEPLOY_DIR/$CODENAME
  for PROJECT in gtirb gtirb-pprinter ddisasm;do
    curl -L https://git.grammatech.com/rewriting/${PROJECT}/-/jobs/artifacts/master/download?job=debian-installer-ubuntu$OS_NUM --output "${PROJECT}-artifacts.zip"
    unzip ${PROJECT}-artifacts.zip
  done
  rm *.zip
  curl http://otsego.grammatech.com/u4/TARBALLS/debloat/pkgs/libcapstone-dev_4.0.1-gt1_amd64.deb --output libcapstone-dev_4.0.1-gt1_amd64.deb
  dpkg-scanpackages . /dev/null | gzip -9c > Packages.gz
  popd
};

setup_repo 16 xenial
setup_repo 18 bionic

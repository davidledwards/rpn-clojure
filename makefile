#
# Copyright 2020 David Edwards
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
VERSION=0.1
PROJECT=rpn-clojure
TARGET=${PROJECT}-${VERSION}
BASE_DIR=$(abspath .)

MAIN_SOURCES=$(wildcard ${BASE_DIR}/src/rpn/*.clj)
TEST_SOURCES=$(wildcard ${BASE_DIR}/test/rpn/*.clj)
SHELL_SOURCES=${wildcard ${BASE_DIR}/src/shell/*}
ALL_SOURCES=${MAIN_SOURCES} ${TEST_SOURCES} ${SHELL_SOURCES}

BUILD_DIR=${BASE_DIR}/build
BUILD_JAR=${BUILD_DIR}/${TARGET}.jar

STAGE_DIR=${BUILD_DIR}/${TARGET}

PACKAGE_TAR_FILE=${TARGET}.tar.gz
PACKAGE_TAR=${BUILD_DIR}/${PACKAGE_TAR_FILE}
PACKAGE_ZIP_FILE=${TARGET}.zip
PACKAGE_ZIP=${BUILD_DIR}/${PACKAGE_ZIP_FILE}

help:
	@echo "useful targets:"
	@echo "  all       performs clean, build, package"
	@echo "  clean     remove all transient build files"
	@echo "  build     runs automated tests and creates uberjar"
	@echo "  package   creates packages for distribution"

all: clean build package

clean:
	rm -rf ${BUILD_DIR}

${BUILD_JAR}: ${MAIN_SOURCES} ${TEST_SOURCES}
	clj -M:test
	clj -M:build --target $@

build: ${BUILD_JAR}

${PACKAGE_TAR}: ${BUILD_JAR} ${SHELL_SOURCES}
	mkdir -p ${STAGE_DIR}/lib
	cp ${BUILD_JAR} ${STAGE_DIR}/lib
	cp ${SHELL_SOURCES} ${STAGE_DIR}
	tar czf $@ -C ${BUILD_DIR} ${TARGET}

${PACKAGE_ZIP}: ${BUILD_DIR} ${SHELL_SOURCES}
	mkdir -p ${STAGE_DIR}/lib
	cp ${BUILD_JAR} ${STAGE_DIR}/lib
	cp ${SHELL_SOURCES} ${STAGE_DIR}
	(cd ${BUILD_DIR} && zip -r ${PACKAGE_ZIP_FILE} ${TARGET})

package: ${PACKAGE_TAR} ${PACKAGE_ZIP}

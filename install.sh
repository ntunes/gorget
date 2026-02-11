#!/bin/sh
# Gorget installer — https://github.com/ntunes/gorget
# Usage:
#   curl -fsSL https://raw.githubusercontent.com/ntunes/gorget/main/install.sh | sh
#   curl -fsSL https://raw.githubusercontent.com/ntunes/gorget/main/install.sh | VERSION=v0.1.0 sh
set -eu

REPO="ntunes/gorget"
INSTALL_DIR="${HOME}/.gorget/bin"

main() {
    detect_platform
    resolve_version
    download_and_install
    setup_path
    echo ""
    echo "Gorget ${VERSION} installed to ${INSTALL_DIR}/gg"
    echo ""
    echo "Run 'gg --help' to get started."
    echo "(You may need to restart your shell or run 'source ~/.${DETECTED_SHELL}rc' first.)"
}

detect_platform() {
    OS="$(uname -s)"
    ARCH="$(uname -m)"

    case "${OS}" in
        Linux)  OS_TARGET="unknown-linux-musl" ;;
        Darwin) OS_TARGET="apple-darwin" ;;
        *)      err "Unsupported OS: ${OS}" ;;
    esac

    case "${ARCH}" in
        x86_64|amd64)  ARCH_TARGET="x86_64" ;;
        aarch64|arm64) ARCH_TARGET="aarch64" ;;
        *)             err "Unsupported architecture: ${ARCH}" ;;
    esac

    TARGET="${ARCH_TARGET}-${OS_TARGET}"
    echo "Detected platform: ${TARGET}"
}

resolve_version() {
    if [ -n "${VERSION:-}" ]; then
        echo "Using specified version: ${VERSION}"
        return
    fi

    echo "Fetching latest release..."
    VERSION=$(curl -fsSL "https://api.github.com/repos/${REPO}/releases/latest" \
        | grep '"tag_name"' \
        | head -1 \
        | sed 's/.*"tag_name": *"//;s/".*//')

    if [ -z "${VERSION}" ]; then
        err "Could not determine latest version. Set VERSION env var to install a specific version."
    fi
    echo "Latest version: ${VERSION}"
}

download_and_install() {
    TARBALL="gg-${VERSION}-${TARGET}.tar.gz"
    URL="https://github.com/${REPO}/releases/download/${VERSION}/${TARBALL}"

    echo "Downloading ${URL}..."

    TMPDIR=$(mktemp -d)
    trap 'rm -rf "${TMPDIR}"' EXIT

    curl -fsSL "${URL}" -o "${TMPDIR}/${TARBALL}"
    tar xzf "${TMPDIR}/${TARBALL}" -C "${TMPDIR}"

    mkdir -p "${INSTALL_DIR}"
    mv "${TMPDIR}/gg" "${INSTALL_DIR}/gg"
    chmod +x "${INSTALL_DIR}/gg"
}

setup_path() {
    SHELL_NAME="$(basename "${SHELL:-/bin/sh}")"
    DETECTED_SHELL="${SHELL_NAME}"
    PATH_LINE="export PATH=\"\${HOME}/.gorget/bin:\${PATH}\""

    case "${SHELL_NAME}" in
        bash) RC_FILE="${HOME}/.bashrc" ;;
        zsh)  RC_FILE="${HOME}/.zshrc" ;;
        fish)
            # Fish uses a different syntax
            FISH_DIR="${HOME}/.config/fish"
            RC_FILE="${FISH_DIR}/config.fish"
            PATH_LINE="fish_add_path \${HOME}/.gorget/bin"
            ;;
        *)    RC_FILE="${HOME}/.profile" ; DETECTED_SHELL="profile" ;;
    esac

    # Check if PATH already includes the install dir
    case ":${PATH}:" in
        *":${INSTALL_DIR}:"*) return ;;
    esac

    # Check if rc file already has the line
    if [ -f "${RC_FILE}" ] && grep -qF '.gorget/bin' "${RC_FILE}" 2>/dev/null; then
        return
    fi

    echo "" >> "${RC_FILE}"
    echo "# Gorget" >> "${RC_FILE}"
    echo "${PATH_LINE}" >> "${RC_FILE}"
    echo "Added ${INSTALL_DIR} to PATH in ${RC_FILE}"
}

err() {
    echo "Error: $1" >&2
    exit 1
}

main

#!/bin/env bash

# NOTE: This file is generated from "config-literate.org".
# This shell script has been generated from the litterate Org configuration.
# It helps installing required tools (for Arch/Manjaro Linux) and tweak some
# system settings

update-mime-database ~/.local/share/mime

xdg-mime default emacs-client.desktop text/org

update_appimageupdatetool () {
  TOOL_NAME=appimageupdatetool
  MACHINE_ARCH=$(uname -m)
  APPIMAGE_UPDATE_TOOL_PATH="$HOME/.local/bin/${TOOL_NAME}"
  APPIMAGE_UPDATE_TOOL_URL="https://github.com/AppImage/AppImageUpdate/releases/download/continuous/${TOOL_NAME}-${MACHINE_ARCH}.AppImage"

  if [ -f "${APPIMAGE_UPDATE_TOOL_PATH}" ] && "$APPIMAGE_UPDATE_TOOL_PATH" -j "${APPIMAGE_UPDATE_TOOL_PATH}" 2&>/dev/null; then
    echo "${TOOL_NAME} already up to date"
  else
    if [ -f "${APPIMAGE_UPDATE_TOOL_PATH}" ]; then
      echo "Update available, downloading latest ${MACHINE_ARCH} version to ${APPIMAGE_UPDATE_TOOL_PATH}"
      mv "${APPIMAGE_UPDATE_TOOL_PATH}" "${APPIMAGE_UPDATE_TOOL_PATH}.backup"
    else
      echo "${TOOL_NAME} not found, downloading latest ${MACHINE_ARCH} version to ${APPIMAGE_UPDATE_TOOL_PATH}"
    fi
    wget -O "${APPIMAGE_UPDATE_TOOL_PATH}" "${APPIMAGE_UPDATE_TOOL_URL}" && # 2&>/dev/null
        echo "Downloaded ${TOOL_NAME}-${MACHINE_ARCH}.AppImage" &&
        [ -f "${APPIMAGE_UPDATE_TOOL_PATH}.backup" ] &&
        rm "${APPIMAGE_UPDATE_TOOL_PATH}.backup"
    chmod a+x "${APPIMAGE_UPDATE_TOOL_PATH}"
  fi
}

update_appimageupdatetool

if [ ! -d "$HOME/.oh-my-zsh" ]; then
  unset INSTALL_CONFIRM
  read -p "Do you want install Oh-my-Zsh [Y | N]: " INSTALL_CONFIRM
  if [[ "$INSTALL_CONFIRM" =~ "^[Yy]$" ]]; then
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
  fi
fi

if [ ! -d "$HOME/.oh-my-bash" ]; then
  unset INSTALL_CONFIRM
  read -p "Do you want install Oh-my-Bash [Y | N]: " INSTALL_CONFIRM
  if [[ "$INSTALL_CONFIRM" =~ "^[Yy]$" ]]; then
    bash -c "$(curl -fsSL https://raw.githubusercontent.com/ohmybash/oh-my-bash/master/tools/install.sh)"
  fi
fi

update_apache_tika () {
  TIKA_JAR_PATH="$HOME/.local/share/tika"

  if [ ! -d "${TIKA_JAR_PATH}" ]; then
    mkdir -p "${TIKA_JAR_PATH}"
  fi

  TIKA_BASE_URL=https://archive.apache.org/dist/tika/
  TIKA_JAR_LINK="${TIKA_JAR_PATH}/tika-app.jar"

  echo -n "Checking for new Apache Tika App version... "

  command -v pandoc >/dev/null || echo "Cannot check, pandoc is missing!"; return

  # Get the lastest version
  TIKA_VERSION=$(
    curl -s "${TIKA_BASE_URL}" | # Get the page
    pandoc -f html -t plain | # Convert HTML page to plain text.
    awk '/([0-9]+\.)+[0-1]\// {print substr($1, 0, length($1)-1)}' | # Get the versions directories (pattern: X.X.X/)
    sort -rV | # Sort versions, the newest first
    head -n 1 # Get the first (newest) version
  )

  if [ -z "${TIKA_VERSION}" ]; then
    echo "Failed, check your internet connection."
    exit 1
  fi

  echo "Lastest version is ${TIKA_VERSION}"

  TIKA_JAR="${TIKA_JAR_PATH}/tika-app-${TIKA_VERSION}.jar"
  TIKA_JAR_URL="${TIKA_BASE_URL}${TIKA_VERSION}/tika-app-${TIKA_VERSION}.jar"

  if [ ! -f "${TIKA_JAR}" ]; then
    echo "New version available!"
    unset INSTALL_CONFIRM
    read -p "Do you want to download Apache Tika App v${TIKA_VERSION}? [Y | N]: " INSTALL_CONFIRM
    if [[ "$INSTALL_CONFIRM" == "Y" ]]; then
      curl -o "${TIKA_JAR}" "${TIKA_JAR_URL}" && echo "Apache Tika App v${TIKA_VERSION} downloaded successfully"
    fi
  else
    echo "Apache Tika App is up-to-date, version ${TIKA_VERSION} already downloaded to '${TIKA_JAR}'"
  fi

  # Check the existance of the symbolic link
  if [ -L "${TIKA_JAR_LINK}" ]; then
    unlink "${TIKA_JAR_LINK}"
  fi

  # Create a symbolic link to the installed version
  ln -s "${TIKA_JAR}" "${TIKA_JAR_LINK}"
}

update_apache_tika

if ! command -v nvm >/dev/null; then
  unset INSTALL_CONFIRM
  read -p "Do you want install nvm [Y | N]: " INSTALL_CONFIRM
  if [[ "$INSTALL_CONFIRM" =~ "^[Yy]$" ]]; then
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.5/install.sh | bash
  fi
fi

if ! command -v pyenv &>/dev/null; then
  unset INSTALL_CONFIRM
  read -p "Do you want install pyenv [Y | N]: " INSTALL_CONFIRM
  if [[ "$INSTALL_CONFIRM" =~ "^[Yy]$" ]]; then
    curl https://pyenv.run | bash
    # Install pyenv plugins
    for plugin in virtualenv users ccache update pip-migrate doctor version-ext; do
        git clone https://github.com/pyenv/pyenv-virtualenv.git "${HOME}/.pyenv/plugins/pyenv-${plugin}"
    done
  fi
fi

if ! command -v uv &>/dev/null; then
  unset INSTALL_CONFIRM
  read -p "Do you want install uv [Y | N]: " INSTALL_CONFIRM
  if [[ "$INSTALL_CONFIRM" =~ "^[Yy]$" ]]; then
    curl -LsSf https://astral.sh/uv/install.sh | sh
  fi
fi

# if ! command -v nix &> /dev/null; then
#   unset INSTALL_CONFIRM
#   read -p "Do you want install Nix for all users? [Y | N]: " INSTALL_CONFIRM

#   if [[ "$INSTALL_CONFIRM" == "Y" ]]; then
#     sh <(curl -L https://nixos.org/nix/install) --daemon
#   else
#     read -p "Do you want install Nix for the current user only? [Y | N]: " INSTALL_CONFIRM

#     if [[ "$INSTALL_CONFIRM" == "Y" ]]; then
#       sh <(curl -L https://nixos.org/nix/install) --no-daemon
#     fi
#   fi
# fi

# if ! command -v guix &>/dev/null; then
#   unset INSTALL_CONFIRM
#   read -p "Do you want install guix [Y | N]: " INSTALL_CONFIRM
#   if [[ "$INSTALL_CONFIRM" =~ "^[Yy]$" ]]; then
#     curl -sfL https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh | sudo bash
#   fi
# fi

if ! command -v direnv &>/dev/null; then
  unset INSTALL_CONFIRM
  read -p "Do you want install direnv [Y | N]: " INSTALL_CONFIRM
  if [[ "$INSTALL_CONFIRM" =~ "^[Yy]$" ]]; then
    curl -sfL https://direnv.net/install.sh | bash
  fi
fi

check_and_install_pkg() {
    PKG_NAME="$1"
    if ! pacman -Qiq "${PKG_NAME}" &>/dev/null; then
        echo "Package ${PKG_NAME} is missing, installing it using yay"
        yay -S --noconfirm "${PKG_NAME}"
    fi
}

PKGS_LIST=(
    # System tools
    git repo ripgrep fd gnupg fzf the_silver_searcher xsel xorg-xhost
    neovim ecryptfs-utils libvterm binutils
    # Fonts
    ttf-martian-mono ttf-ttf-ibm-plex ttf-fira-code ttf-roboto-mono ttf-overpass
    ttf-lato ttf-input ttf-cascadia-code ttf-jetbrains-mono
    ttf-fantasque-sans-mono ttc-iosevka ttc-iosevka-slab ttc-iosevka-curly
    ttc-iosevka-curly-slab
    # Programming tools
    ccls cppcheck clang gcc gcc-m2 gcc-rust gdb lldb valgrind rr openocd vls vlang
    rustup semgrep-bin pylyzer-git zig
    # Lisp/Scheme
    sbcl cmucl clisp chez-scheme mit-scheme chibi-scheme chicken gcl
    # Math
    maxima octave scilab-bin graphviz jupyterlab jupyter-notebook r
    # Media
    mpc mpv mpd vlc yt-dlp poppler ffmpegthumbnailer mediainfo imagemagick
    # Email
    mu isync msmtp
    # Documents
    djvulibre catdoc unrtf perl-image-exiftool wkhtmltopdf pandoc hugo inkscape
    imagemagick
    # Natural languages
    aspell aspell-en aspell-fr aspell-ar grammalecte language-tool ltex-ls-bin sdcv
    # Apps
    brave zotero
)

if command -v pacman >/dev/null; then
    unset INSTALL_CONFIRM
    read -p "Do you want to Arch Linux packages? [Y | N]: " INSTALL_CONFIRM
    if [[ "$INSTALL_CONFIRM" =~ "^[Yy]$" ]]; then
        for PKG in "${PKGS_LIST[@]}"; do
            check_and_install_pkg "$PKG"
        done
    fi
else
    echo "Not on Arch Linux or Manjaro"
fi

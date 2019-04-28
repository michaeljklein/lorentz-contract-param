#
# © 2019 Tocqueville Group
#
# SPDX-License-Identifier: AGPL-3.0-or-later
#

{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "myEnv";
  buildInputs = [ zlib ];
  buildPhase = ''
    export LANG=en_US.UTF-8
    '';
}

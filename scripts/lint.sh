#!/usr/bin/env bash

#
# © 2019 Tocqueville Group
#
# SPDX-License-Identifier: AGPL-3.0-or-later
#

hlint --hint .hlint.yaml --hint .hlint-universum.yaml src

ex=$?

if [ $ex != 0 ]; then
  echo ''
  echo '====================================================================='
  echo ''
  echo 'Note: to ignore a particular hint (e.g. "Reduce duplication"), write'
  echo 'this in the source file:'
  echo ''
  tput bold
  echo '{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}'
  tput sgr0
  echo ''
  echo 'You can also apply it just to a particular function, which is better:'
  echo ''
  tput bold
  echo '{-# ANN funcName ("HLint: ignore Reduce duplication" :: Text) #-}'
  tput sgr0
  echo ''

  exit $ex
fi

#
# © 2019 Tocqueville Group
#
# SPDX-License-Identifier: AGPL-3.0-or-later
#

FROM alpine
RUN mkdir -p /opt/morley/
WORKDIR /opt/morley
COPY ./tmp/morley ./morley
ENV PATH "$PATH:/opt/morley"

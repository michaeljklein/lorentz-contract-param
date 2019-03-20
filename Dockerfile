FROM haskell as build
# Workaround required for statically linked binary building
# See https://www.fpcomplete.com/blog/2016/10/static-compilation-with-stack
WORKDIR /usr/lib/gcc/x86_64-linux-gnu/6/
RUN cp crtbeginT.o crtbeginT.o.orig
RUN cp crtbeginS.o crtbeginT.o

WORKDIR /project
COPY . /project
RUN stack build --ghc-options '-optl-static -fPIC' --copy-bins

FROM alpine
RUN mkdir -p /opt/morley/
WORKDIR /opt/morley
COPY --from=build /root/.local/bin/morley ./morley
ENV PATH "$PATH:/opt/morley"
